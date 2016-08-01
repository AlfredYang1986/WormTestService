package module.paraconfig

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object ConfigModule {
  
    def setupDefaultConfig = {
        val builder = MongoDBObject.newBuilder
       
        builder += "index" -> 0
        builder += "resource_type" ->  MongoDBList.newBuilder.result
        builder += "patient_type" ->  MongoDBList.newBuilder.result
        
        _data_connection.getCollection("config") += builder.result
    }
  
    def pushResourceType(data : JsValue) : JsValue =
        try {
            val resource = (data \ "resource").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
          
            (from db() in "config" where ("index" -> 0) select (x => x)).toList match {
              case head :: Nil => {
                val resource_lst = head.getAs[MongoDBList]("resource_type").get.toList.asInstanceOf[List[String]]
                val new_lst = (resource_lst :+ resource).distinct
                head += "resource_type" -> new_lst
                _data_connection.getCollection("config").update(DBObject("index" -> 0), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushResourceType"), "result" -> toJson("push config success")))
              }
              case Nil => {
                setupDefaultConfig
                pushResourceType(data)
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def queryResourceType(data : JsValue) : JsValue =
        try {
            (from db() in "config" where ("index" -> 0) select (x => x)).toList match {
              case head :: Nil => {
                val resource_lst = head.getAs[MongoDBList]("resource_type").get.toList.asInstanceOf[List[String]]
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("queryResourceType"), "result" -> toJson(resource_lst)))
              }
              case Nil => {
                setupDefaultConfig
                queryResourceType(data)
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def popResourceType(data : JsValue) : JsValue =
        try {
            val resource = (data \ "resource").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
          
            (from db() in "config" where ("index" -> 0) select (x => x)).toList match {
              case head :: Nil => {
                val resource_lst = head.getAs[MongoDBList]("resource_type").get.toList.asInstanceOf[List[String]]
                head += "resource_type" -> resource_lst.filterNot (_.equals(resource)) 
                _data_connection.getCollection("config").update(DBObject("index" -> 0), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("popResourceType"), "result" -> toJson("pop config success")))
              }
              case Nil => {
                setupDefaultConfig
                popResourceType(data)
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage);
        }
        
    def pushPatientType(data : JsValue) : JsValue =
        try {
            val patient = (data \ "patient").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
          
            (from db() in "config" where ("index" -> 0) select (x => x)).toList match {
              case head :: Nil => {
                val patient_lst = head.getAs[MongoDBList]("patient_type").get.toList.asInstanceOf[List[String]]
                val new_lst = (patient_lst :+ patient).distinct
                head += "patient_type" -> new_lst
                _data_connection.getCollection("config").update(DBObject("index" -> 0), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushPatientType"), "result" -> toJson("push config success")))
              }
              case Nil => {
                setupDefaultConfig
                pushPatientType(data)
              }
              case _ => ???

            } 
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
     
    def popPatientType(data : JsValue) : JsValue =
        try {
            val patient = (data \ "patient").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
          
            (from db() in "config" where ("index" -> 0) select (x => x)).toList match {
              case head :: Nil => {
                val patient_lst = head.getAs[MongoDBList]("patient_type").get.toList.asInstanceOf[List[String]]
                head += "patient_type" -> patient_lst.filterNot (_.equals(patient))
                _data_connection.getCollection("config").update(DBObject("index" -> 0), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("popPatientType"), "result" -> toJson("pop config success")))
              }
              case Nil => {
                setupDefaultConfig
                pushPatientType(data)
              }
              case _ => ???
            }            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def queryPatientType(data : JsValue) : JsValue =
        try {
            (from db() in "config" where ("index" -> 0) select (x => x)).toList match {
              case head :: Nil => {
                val patient_lst = head.getAs[MongoDBList]("patient_type").get.toList.asInstanceOf[List[String]]
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("queryPatientType"), "result" -> toJson(patient_lst)))
              }
              case Nil => {
                setupDefaultConfig
                queryResourceType(data)
              }
              case _ => ???
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
}