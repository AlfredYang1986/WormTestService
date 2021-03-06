package module.sample

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.patient.PatientModule

import java.util.Date

object sampleStatus {
    case object pushed extends sampleStatusDefines(0, "pushed")
    case object tested extends sampleStatusDefines(1, "tested")
    case object adjuested extends sampleStatusDefines(2, "adjusted")
    case object printed extends sampleStatusDefines(3, "printed")
}

sealed abstract class sampleStatusDefines(val t : Int, val des : String)

object SampleModule {
  
    def DB2JsValue(obj : MongoDBObject) : JsValue = {
        val patient_id = obj.getAs[String]("patient_id").get
        val patient = (PatientModule.queryPatientWithId(toJson(Map("patient_id" -> patient_id))) \ "result").asOpt[JsValue].get
        toJson(Map("sample_id" -> toJson(obj.getAs[String]("sample_id").get),
                   "patient_id" -> toJson(obj.getAs[String]("patient_id").get),
                   "resource" -> toJson(obj.getAs[String]("resource").get),
                   "date" -> toJson(obj.getAs[Number]("date").get.longValue),
                   "images" -> toJson(obj.getAs[MongoDBList]("images").get.toList.asInstanceOf[List[String]]),
                   "result" -> toJson(obj.getAs[MongoDBList]("result").get.toList.asInstanceOf[List[String]]),
                   
                   "start_date" -> toJson(obj.getAs[Number]("start_date").get.longValue),
                   "end_date" -> toJson(obj.getAs[Number]("end_date").get.longValue),
                   "pre_test_date" -> toJson(obj.getAs[Number]("pre_test_date").get.longValue),
                   "testing_date" -> toJson(obj.getAs[Number]("testing_date").get.longValue),
                   "reporting_date" -> toJson(obj.getAs[Number]("reporting_date").get.longValue),
                  
                   "query_doctor" -> toJson(obj.getAs[String]("query_doctor").get),
                   "pre_test_doctor" -> toJson(obj.getAs[String]("pre_test_doctor").get),
                   "testing_doctor" -> toJson(obj.getAs[String]("testing_doctor").get),
                   "post_test_doctor" -> toJson(obj.getAs[String]("post_test_doctor").get),
                  
                   "status" -> toJson(obj.getAs[Number]("status").get.intValue),
                   "section" -> toJson(obj.getAs[String]("section").get),
                   "index" -> toJson(obj.getAs[Int]("index").get.intValue),

                   "index_of_day" -> toJson(obj.getAs[Int]("index_of_day").get.intValue),
                   
                   "patient" -> toJson(patient)))
    }
  
    def updateDBObject(data : JsValue, obj : MongoDBObject) : MongoDBObject = {
        (data \ "patient_id").asOpt[String].map (tmp => obj += "patient_id" -> tmp).getOrElse(Unit)
        (data \ "status").asOpt[Int].map(tmp => obj += "status" -> tmp.asInstanceOf[Number]).getOrElse(Unit)
        (data \ "resource").asOpt[String].map(tmp => obj += "resource" -> tmp).getOrElse(Unit)
        
        (data \ "start_date").asOpt[Long].map (tmp => obj += "start_date" -> tmp.asInstanceOf[Number]).getOrElse(Unit)
        (data \ "end_date").asOpt[Long].map (tmp => obj += "end_date" -> tmp.asInstanceOf[Number]).getOrElse(Unit)
        (data \ "pre_test_date").asOpt[Long].map (tmp => obj += "pre_test_date" -> tmp.asInstanceOf[Number]).getOrElse(Unit)
        (data \ "testing_date").asOpt[Long].map (tmp => obj += "testing_date" -> tmp.asInstanceOf[Number]).getOrElse(Unit)
        (data \ "reporting_date").asOpt[Long].map (tmp => obj += "reporting_date" -> tmp.asInstanceOf[Number]).getOrElse(Unit)
           
        (data \ "query_doctor").asOpt[String].map (tmp => obj += "query_doctor" -> tmp).getOrElse(Unit)
        (data \ "pre_test_doctor").asOpt[String].map (tmp => obj += "pre_test_doctor" -> tmp).getOrElse(Unit)
        (data \ "testing_doctor").asOpt[String].map (tmp => obj += "testing_doctor" -> tmp).getOrElse(Unit)
        (data \ "post_test_doctor").asOpt[String].map (tmp => obj += "post_test_doctor" -> tmp).getOrElse(Unit)

        (data \ "section").asOpt[String].map (tmp => obj += "section" -> tmp).getOrElse(Unit)
        (data \ "index").asOpt[Int].map (tmp => obj += "index" -> tmp.asInstanceOf[Number]).getOrElse(Unit)

        obj
    }
  
    def JsValue2DBObject(data : JsValue) : Option[MongoDBObject] = {
        val builder = MongoDBObject.newBuilder
        try {
            (data \ "sample_id").asOpt[String].map (tmp => builder += "sample_id" -> tmp).getOrElse(throw new Exception("sample is essential"))
            (data \ "status").asOpt[Int].map(tmp => builder += "status" -> tmp.asInstanceOf[Number]).getOrElse(builder += "status" -> sampleStatus.pushed.t)
            (data \ "resource").asOpt[String].map(tmp => builder += "resource" -> tmp).getOrElse(builder += "resource" -> "")
           
            (data \ "start_date").asOpt[Long].map (tmp => builder += "start_date" -> tmp).getOrElse(builder += "start_date" -> 0.longValue)
            (data \ "end_date").asOpt[Long].map (tmp => builder += "end_date" -> tmp).getOrElse(builder += "end_date" -> 0.longValue)
            (data \ "pre_test_date").asOpt[Long].map (tmp => builder += "pre_test_date" -> tmp).getOrElse(builder += "pre_test_date" -> 0.longValue)
            (data \ "testing_date").asOpt[Long].map (tmp => builder += "testing_date" -> tmp).getOrElse(builder += "testing_date" -> 0.longValue)
            (data \ "reporting_date").asOpt[Long].map (tmp => builder += "reporting_date" -> tmp).getOrElse(builder += "reporting_date" -> 0.longValue)
           
            (data \ "query_doctor").asOpt[String].map (tmp => builder += "query_doctor" -> tmp).getOrElse(builder += "query_doctor" -> "")
            (data \ "pre_test_doctor").asOpt[String].map (tmp => builder += "pre_test_doctor" -> tmp).getOrElse(builder += "pre_test_doctor" -> "")
            (data \ "testing_doctor").asOpt[String].map (tmp => builder += "testing_doctor" -> tmp).getOrElse(builder += "testing_doctor" -> "")
            (data \ "post_test_doctor").asOpt[String].map (tmp => builder += "post_test_doctor" -> tmp).getOrElse(builder += "post_test_doctor" -> "")

            (data \ "treatment").asOpt[String].map (tmp => builder += "treatment" -> tmp).getOrElse(builder += "treatment" -> "")
            (data \ "advise").asOpt[String].map (tmp => builder += "advise" -> tmp).getOrElse(builder += "advise" -> "")
            (data \ "des").asOpt[String].map (tmp => builder += "des" -> tmp).getOrElse(builder += "des" -> "")
            (data \ "section").asOpt[String].map (tmp => builder += "section" -> tmp).getOrElse(builder += "section" -> "")
            (data \ "index").asOpt[Int].map (tmp => builder += "index" -> tmp).getOrElse(builder += "index" -> 0)
           
            val date = new Date().getTime / (24 * 60 * 60 * 1000)
            builder += "index_of_day" -> (from db() in "sample" where ("date" -> date) select (x => x)).count
            builder += "status" -> sampleStatus.pushed.t
            builder += "date" -> date
            builder += "images" -> MongoDBList.newBuilder.result
            builder += "result" -> MongoDBList.newBuilder.result

            (data \ "patient_id").asOpt[String].map { tmp => 
                (data \ "patient").asOpt[JsValue].map (x => PatientModule.updatePaient(x)).getOrElse(throw new Exception(""))
                builder += "patient_id" -> tmp
            }.getOrElse {
                val result = (data \ "patient").asOpt[JsValue].map (x => PatientModule.updatePaient(x)).getOrElse(throw new Exception(""))
                builder += "patient_id" -> (result \ "result" \ "patient_id").asOpt[String].get
            }
            
            Some(builder.result)
        } catch {
          case ex : Exception => None
        }
    }
  
    def pushSample(data : JsValue) : JsValue = {
        def pushSampleImpl = JsValue2DBObject(data) match {
                                case Some(x) => {
                                  _data_connection.getCollection("sample") += x
                                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushSample"), "result" -> toJson("success")))
                                }
                                case None => toJson(Map("status" -> toJson("error"), "error" -> toJson("push sample error")))
                             }
      
        val sample_id = (data \ "sample_id").asOpt[String].get
        (from db() in "sample" where ("sample_id" -> sample_id) select (x => x)).toList match {
          case Nil => pushSampleImpl
          case head :: Nil => updateSample(data)
          case _ => ErrorCode.errorToJson("sample exist")
        }
    }
    
    def updateSample(data : JsValue) : JsValue = 
        try {
            val sample_id = (data \ "sample_id").asOpt[String].map (x => x).getOrElse("sample not exist")
            (from db() in "sample" where ("sample_id" -> sample_id) select (x => x)).toList match {
              case Nil => pushSample(data)
              case head :: Nil => {
                  val result = updateDBObject(data, head)
                  _data_connection.getCollection("sample").update(DBObject("sample_id" -> sample_id), result)
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("updateSample"), "result" -> toJson(DB2JsValue(head))))
              }
              case _ => ErrorCode.errorToJson("sample exist")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def pushSampleImage(data : JsValue) : JsValue =
        try {
            val sample_id = (data \ "sample_id").asOpt[String].map (x => x).getOrElse(throw new Exception("sample not exist"))
            val image = (data \ "image").asOpt[String].map (x => x).getOrElse(throw new Exception("image not set"))
            
            (from db() in "sample" where ("sample_id" -> sample_id) select (x => x)).toList match {
              case Nil => throw new Exception("sample not exist")
              case head :: Nil => {
                  val lst = head.getAs[MongoDBList]("images").get
                  lst += image
                  _data_connection.getCollection("sample").update(DBObject("sample_id" -> sample_id), head)
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushSampleImage"), "result" -> toJson("update success")))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def popSampleImage(data : JsValue) : JsValue = {
        try {
            val sample_id = (data \ "sample_id").asOpt[String].map (x => x).getOrElse(throw new Exception("sample not exist"))
            val image = (data \ "image").asOpt[String].map (x => x).getOrElse(throw new Exception("image not set"))
            
            (from db() in "sample" where ("sample_id" -> sample_id) select (x => x)).toList match {
              case Nil => throw new Exception("sample not exist")
              case head :: Nil => {
                  var lst = head.getAs[MongoDBList]("images").get
                  head += "images" -> lst.filterNot(x => image.equals(x.asInstanceOf[String])).toList
                  _data_connection.getCollection("sample").update(DBObject("sample_id" -> sample_id), head)
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("popSampleImage"), "result" -> toJson(Map("sample_id" -> sample_id, "image" -> image))))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def selectSampleImage(data : JsValue) : JsValue = {
        null
    }
    
    def queryNotTestSample(data : JsValue) : JsValue = {
        try {
            val status = sampleStatus.pushed.t
            toJson(Map("status" -> toJson("ok"), "method" -> toJson("queryNotTestSample"), "result" -> toJson(
                  (from db() in "sample" where ("status" -> status) select (DB2JsValue(_))).toList)))
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }

    def queryTestedSample(data : JsValue) : JsValue = {
        try {
            val status = sampleStatus.pushed.t
            val reVal = ((from db() in "sample" where ("status" $ne status) select (DB2JsValue(_))).toList)
            val reVal_s = reVal.sortBy (x => (x \ "status").asOpt[Int].get)
            toJson(Map("status" -> toJson("ok"), "method" -> toJson("queryTestedSample"), "result" -> toJson(reVal_s)))
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def querySampleWithID(data : JsValue) : JsValue = {
        try {
            val sample_id = (data \ "sample_id").asOpt[String].map (x => x).getOrElse(throw new Exception("sample id needed"))
            toJson(Map("status" -> toJson("ok"), "method" -> toJson("querySampleWithID"), "result" -> 
                ((from db() in "sample" where ("sample_id" -> sample_id) select(DB2JsValue(_))).toList match {
                  case Nil => toJson("")
                  case head :: Nil => toJson(head)
                  case _ => ???
                })))
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def sampleTestComplished(data : JsValue) : JsValue = 
        try {
            val sample_id = (data \ "sample_id").asOpt[String].map (x => x).getOrElse(throw new Exception("sample id needed"))
            (from db() in "sample" where ("sample_id" -> sample_id) select (x => x)).toList match {
              case head :: Nil => {
                head += "status" -> sampleStatus.tested.t.asInstanceOf[Number]
                head += "testing_date" -> new Date().getTime.asInstanceOf[Number]
                head += "testing_doctor" -> (data \ "testing_doctor").asOpt[String].map (x => x).getOrElse(throw new Exception("doctor is needed"))
                _data_connection.getCollection("sample").update(DBObject("sample_id" -> sample_id), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("sampleTestComplished"), "result" -> toJson("success")))
              }
              case _ => throw new Exception("")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def sampleTestAdjusted(data : JsValue) : JsValue = 
        try {
            val sample_id = (data \ "sample_id").asOpt[String].map (x => x).getOrElse(throw new Exception("sample id needed"))
            (from db() in "sample" where ("sample_id" -> sample_id) select (x => x)).toList match {
              case head :: Nil => {
                head += "status" -> sampleStatus.adjuested.t.asInstanceOf[Number] 
                head += "reporting_date" -> new Date().getTime.asInstanceOf[Number]
                head += "post_test_doctor" -> (data \ "post_test_doctor").asOpt[String].map (x => x).getOrElse(throw new Exception("doctor is needed"))
                _data_connection.getCollection("sample").update(DBObject("sample_id" -> sample_id), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("sampleTestPublished"), "result" -> toJson("success")))
              }
              case _ => throw new Exception("")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }

    def sampleTestPrinted(data : JsValue) : JsValue = 
        try {
            val sample_id = (data \ "sample_id").asOpt[String].map (x => x).getOrElse(throw new Exception("sample id needed"))
            (from db() in "sample" where ("sample_id" -> sample_id) select (x => x)).toList match {
              case head :: Nil => {
                head += "status" -> sampleStatus.printed.t.asInstanceOf[Number] 
//                head += "reporting_date" -> new Date().getTime.asInstanceOf[Number]
//                head += "post_test_doctor" -> (data \ "post_test_doctor").asOpt[String].map (x => x).getOrElse(throw new Exception("doctor is needed"))
                _data_connection.getCollection("sample").update(DBObject("sample_id" -> sample_id), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("sampleTestPublished"), "result" -> toJson("success")))
              }
              case _ => throw new Exception("")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def samplePushTestResult(data : JsValue) : JsValue =
        try {
            val sample_id = (data \ "sample_id").asOpt[String].map (x => x).getOrElse(throw new Exception("sample id needed"))
            val result = (data \ "result").asOpt[List[String]].map (x => x).getOrElse(throw new Exception(""))
            
            (from db() in "sample" where ("sample_id" -> sample_id) select (x => x)).toList match {
              case head :: Nil => {
                val ori = head.getAs[MongoDBList]("result").get.toList.asInstanceOf[List[String]]
                head += "result" -> result.distinct
                _data_connection.getCollection("sample").update(DBObject("sample_id" -> sample_id), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("samplePushTestResult"), "result" -> toJson("success")))
              }
              case _ => throw new Exception("")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)      
        }
        
   def sampleConditionSearch(data : JsValue) : JsValue = {
       def pushCondition(cur : DBObject, add : DBObject) : DBObject = if (cur != null) $and(cur, add)
                                                                      else add
       def pushOrCondition(cur : DBObject, add : DBObject) : DBObject = if (cur != null) $or(cur, add)
                                                                        else add
                                                                     
       def queryPatient(data : JsValue) : List[String] = {
           var condition : DBObject = null
           (data \ "patient_id").asOpt[String].map(x => condition = pushCondition(condition, "patient_fake_id" $eq x)).getOrElse(Unit)
           (data \ "patient_name").asOpt[String].map(x => condition = pushCondition(condition, "patient_name" $eq x)).getOrElse(Unit)
           (data \ "patient_age").asOpt[Int].map(x => condition = pushCondition(condition, "patient_age" $eq x)).getOrElse(Unit)
             
           if (condition == null) Nil
           else {
               (from db() in "patient" where condition select (x => x.getAs[String]("patient_id").get)).toList match {
                 case Nil => Nil
                 case head :: Nil => head :: Nil
                 case x : List[String] => x
                 case _ => ???
               }
           }
       }
                                                                      
       try {
           var condition : DBObject = null
           (data \ "time").asOpt[Long].map (x => condition = pushCondition(condition, "date" $gte 1 + x / (24 * 60 * 60 * 1000))).getOrElse(Unit)
           (data \ "time_end").asOpt[Long].map (x => condition = pushCondition(condition, "date" $lte 1 + x / (24 * 60 * 60 * 1000))).getOrElse(Unit)
           (data \ "testing_doctor").asOpt[String].map (x => condition = pushCondition(condition, "testing_doctor" $eq x)).getOrElse(Unit)
           (data \ "sample_id").asOpt[String].map (x => condition = pushCondition(condition, "sample_id" $eq x)).getOrElse(Unit)
           (data \ "post_test_doctor").asOpt[String].map (x => condition = pushCondition(condition, "post_test_doctor" $eq x)).getOrElse(Unit)
           (data \ "worm").asOpt[String].map (x => condition = pushCondition(condition, "result" $eq x)).getOrElse(Unit)

           var pc : DBObject = null
           queryPatient(data) map { pid =>
               pc = pushOrCondition(pc, "patient_id" $eq pid)
           }
           if (pc != null) condition = pushCondition(condition, pc)
           else Unit
           
           if (condition == null) ErrorCode.errorToJson("")
           else toJson(Map("status" -> toJson("ok"), "method" -> toJson("sampleConditionSearch"), "result" -> 
                   toJson((from db() in "sample" where condition select (DB2JsValue(_))).toList)))
       } catch {
         case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
       }
   }
}