package module.patient

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

import module.sercurity.Sercurity

object gender {
    case object male extends genderDefines(0, "male")
    case object female extends genderDefines(1, "female")
}

sealed abstract class genderDefines(val t : Int, val des : String)

object PatientModule {
    
    def DB2JsValue(obj : MongoDBObject) : JsValue = 
        toJson(Map("patient_id" -> toJson(obj.getAs[String]("patient_id").get),
                   "patient_name" -> toJson(obj.getAs[String]("patient_name").get),
                   "patient_gender" -> toJson(obj.getAs[Number]("patient_gender").get.intValue),
                   "patient_age" -> toJson(obj.getAs[Number]("patient_age").get.intValue)))
    
    def pushPatient(data : JsValue) : JsValue = {
        
        def pushPatientImpl : MongoDBObject = {
            val builder = MongoDBObject.newBuilder
            
            (data \ "patient_id").asOpt[String].map (x => builder += "patient_id" -> x).getOrElse(throw new Exception("patient_id exist"))
            (data \ "patient_name").asOpt[String].map (x => builder += "patient_name" -> x).getOrElse(throw new Exception("patient_name exist"))
            (data \ "patient_gender").asOpt[Int].map (x => builder += "patient_gender" -> x.asInstanceOf[Number]).getOrElse(builder += "patient_gender" -> gender.male.t)
            (data \ "patient_age").asOpt[Int].map (x => builder += "patient_age" -> x.asInstanceOf[Number]).getOrElse(builder += "patient_age" -> 0)
            
            builder.result
        }
      
        try {
            val patient_id = (data \ "patient_id").asOpt[String].get
      
            (from db() in "patient" where ("patient_id" -> patient_id) select (x => x)).toList match {
              case Nil => {
                  val reVal = pushPatientImpl
                  _data_connection.getCollection("patient") += reVal
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushPatient"), "result" -> toJson(DB2JsValue(reVal))))
              }
              case _ => throw new Exception("patient exist")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def updatePaient(data : JsValue) : JsValue =
        try {
          val patient_id = (data \ "patient_id").asOpt[String].map (x => x).getOrElse(throw new Exception("patient_id needed"))
          
          (from db() in "patient" where ("patient_id" -> patient_id) select (x => x)).toList match {
            case Nil => pushPatient(data)
            case head :: Nil => {
                (data \ "patient_name").asOpt[String].map (x => head += "patient_name" -> x).getOrElse(Unit)
                (data \ "patient_gender").asOpt[Int].map (x => head += "patient_gender" -> x.asInstanceOf[Number]).getOrElse(Unit)
                (data \ "patient_age").asOpt[Int].map (x => head += "patient_age" -> x.asInstanceOf[Number]).getOrElse(Unit)

                _data_connection.getCollection("patient").update(DBObject("patient_id" -> patient_id), head);
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushPatient"), "result" -> toJson(DB2JsValue(head))))
            }
            case _ => throw new Exception("patient not exist")
          }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def queryPatientWithId(data : JsValue) : JsValue = 
        try {
            val patient_id = (data \ "patient_id").asOpt[String].map (x => x).getOrElse(throw new Exception("patient_id needed"))
            (from db() in "patient" where ("patient_id" -> patient_id) select (DB2JsValue(_))).toList match {
              case head :: Nil => toJson(Map("status" -> toJson("ok"), "method" -> toJson("queryPatientWithId"), "result" -> toJson(head)))
              case _ => throw new Exception("patient not exist")
            }
          
        } catch  {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def queryPatient(data : JsValue) : JsValue = 
        try {
            val take = (data \ "take").asOpt[Int].map (x => x).getOrElse(10)
            val skip = (data \ "skip").asOpt[Int].map (x => x).getOrElse(0)
           
            toJson(Map("status" -> toJson("ok"), "method" -> toJson("queryPatient"), "result" -> toJson( 
                ((from db() in "patient").selectSkipTop(skip)(take)("patient_id")(DB2JsValue(_))).toList)))
          
        } catch {
          case ex : Exception => throw new Exception(ex.getMessage)
        }
}