package module.auth

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.http.Writeable
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
import module.sercurity.Sercurity

import java.util.Date

object authTypes {  // auth  indicate which account 
    case object anyBody extends authTypeDefines(-99, "any body")
    case object notAuth extends authTypeDefines(-1, "not auth")

    case object normal_doctor extends authTypeDefines(0, "normal doctor")
    case object adjusted_doctor extends authTypeDefines(1, "adjusted doctor")

    case object worker extends authTypeDefines(2, "worker")
    case object developer extends authTypeDefines(3, "developer")
    
    case object admin extends authTypeDefines(99, "admin")
}

sealed abstract class authTypeDefines(val t : Int, val des : String)

object AuthModule {
  
    def register(data : JsValue) : JsValue = {
        try {
            val user_name = (data \ "user_name").asOpt[String].map (x => x).getOrElse(throw new Exception("need name"))
            val password = (data \ "password").asOpt[String].map (x => x).getOrElse(throw new Exception("need password"))
            val auth = (data \ "auth").asOpt[Int].map (x => x).getOrElse(throw new Exception("need auth"))
           
            (from db() in "users" where ("user_name" -> user_name) select (x => x)).toList match {
              case Nil => {
                  val builder = MongoDBObject.newBuilder
                  val user_id = Sercurity.md5Hash(user_name + Sercurity.getTimeSpanWithMillSeconds)
                  val token = Sercurity.md5Hash(user_id + Sercurity.getTimeSpanWithMillSeconds)
           
                  builder += "user_id" -> user_id
                  builder += "token" -> token
                  builder += "user_name" -> user_name 
                  builder += "password" -> password
                  builder += "auth" -> auth.asInstanceOf[Number]
           
                  _data_connection.getCollection("users") += builder.result
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("register"), "result" -> toJson(userResult(builder.result))))
              }
              case head :: Nil => throw new Exception("user existing")
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
   
    def userResult(x : MongoDBObject) : JsValue =
        toJson(Map("user_id" -> toJson(x.getAs[String]("user_id").get),
                   "token" -> toJson(x.getAs[String]("token").get),
                   "user_name" -> toJson(x.getAs[String]("user_name").get),
                   "auth" -> toJson(x.getAs[Number]("auth").get.intValue)))
   
    def adminMasterCreate = {
        val seed = "Alfred Yang"
        val admin = "admin"
        
        val admin_builder = MongoDBObject.newBuilder
        val user_id = Sercurity.md5Hash(admin + Sercurity.getTimeSpanWithMillSeconds)

        admin_builder += "auth" -> authTypes.admin.t
        admin_builder += "user_id" -> user_id
        admin_builder += "token" -> Sercurity.md5Hash(user_id +Sercurity.getTimeSpanWithMillSeconds)
        admin_builder += "user_name" -> admin
        admin_builder += "password" -> "admin"
        
        _data_connection.getCollection("users") += admin_builder.result
    }
    
    def popUser(data : JsValue) : JsValue = {
        try {
            val user_name = (data \ "user_name").asOpt[String].map (x => x).getOrElse(throw new Exception("need user name"))
         
            (from db() in "users" where ("user_name" -> user_name) select (x => x)).toList match {
              case Nil => throw new Exception("not existing")
              case head :: Nil => {
                _data_connection.getCollection("users") -= head
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("popUser"), "result" -> toJson(userResult(head))))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def changeStatus(data : JsValue) : JsValue = {
        try {
            val user_name = (data \ "user_name").asOpt[String].map (x => x).getOrElse(throw new Exception("need user name"))
            val auth = (data \ "auth").asOpt[Int].map (x => x).getOrElse(throw new Exception("need auth"))
        
            (from db() in "users" where ("user_name" -> user_name) select (x => x)).toList match { 
              case Nil => throw new Exception("not existing")
              case head :: Nil => {
                head += "auth" -> auth.asInstanceOf[Number]
                _data_connection.getCollection("users").update(DBObject("user_name" -> user_name), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("changeStatus"), "result" -> toJson(userResult(head))))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def changePassword(data : JsValue) : JsValue = {
        try {
            val user_name = (data \ "user_name").asOpt[String].map (x => x).getOrElse(throw new Exception("need user name"))
            val password = (data \ "password").asOpt[String].map (x => x).getOrElse(throw new Exception("need password"))
        
            (from db() in "users" where ("user_name" -> user_name) select (x => x)).toList match { 
              case Nil => throw new Exception("not existing")
              case head :: Nil => {
                head += "password" -> password
                _data_connection.getCollection("users").update(DBObject("user_name" -> user_name), head)
                toJson(Map("status" -> toJson("ok"), "method" -> toJson("changePassword"), "result" -> toJson(userResult(head))))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
                   
//    def login(open_id : String, user_id: String, data : JsValue) : JsValue = {
    def login(data : JsValue) : JsValue =
        try {
            val user_name = (data \ "user_name").asOpt[String].map (x => x).getOrElse(throw new Exception("need name name"))
            val password = (data \ "password").asOpt[String].map (x => x).getOrElse(throw new Exception("need pwd"))
      
            (from db() in "users" where ("user_name" -> user_name, "password" -> password) select (x => x)).toList match {
              case Nil => throw new Exception("not exist")
              case head :: Nil => 
                  toJson(Map("status" -> toJson("ok"), 
                             "method" -> toJson("login"),
                             "result" -> toJson(userResult(head))))
              case _ => throw new Exception
            }
            
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
   
    def lstDoctors(data : JsValue) : JsValue = {
        val auth = (data \ "auth").asOpt[Int].map (x => x).getOrElse(authTypes.normal_doctor.t)
       
        toJson(Map("status" -> toJson("ok"), "mothod" -> toJson("lstDoctor"), "result" -> toJson(
            (from db() in "users" where ("auth" -> auth) select (x => x.getAs[String]("user_name"))).toList)))
    }
        
    def queryProfile(open_id : String, user_id : String, data : JsValue) : JsValue = {
        val query_open_id = (data \ "query_open_id").asOpt[String].map (x => x).getOrElse("")
     
        if (query_open_id == "") ErrorCode.errorToJson("error input")
        else {
            toJson(Map("status" -> toJson("ok"), "result" -> toJson(
                (from db() in "user_profile" where ("open_id" -> query_open_id) select (x => x)).toList match {
                  case Nil => ErrorCode.errorToJson("error input")
                  case head :: Nil => userResult(head)
                  case _ => ???
                })))
        }
    }
    
    def updateProfile(open_id : String, user_id : String, data : JsValue) : JsValue = {
        (from db() in "users" where ("user_id" -> user_id) select (x => x)).toList match {
          case head :: Nil => {
              (data \ "pwd").asOpt[String].map { x => 
                 head += "pwd" -> x 
                 val email = head.getAs[String]("email").get
                 head += "token" -> Sercurity.md5Hash(email + x) 
              }.getOrElse(Unit)
//              (data \ "name").asOpt[String].map (x => head += "name" -> x).getOrElse(Unit)
//              (data \ "register_id").asOpt[String].map (x => head += "register_id" -> x).getOrElse(Unit)
//              (data \ "id_type").asOpt[Int].map (x => head += "id_type" -> x.asInstanceOf[Number]).getOrElse(Unit)
//              (data \ "status").asOpt[Int].map (x => head += "status" -> x.asInstanceOf[Number]).getOrElse(Unit)
//              (data \ "approved_date").asOpt[Long].map (x => head += "approved_date" -> x.asInstanceOf[Number]).getOrElse(Unit)
              
              _data_connection.getCollection("users").update(DBObject("user_id" -> user_id), head)

//              toJson(Map("status" -> toJson("ok"), "result" -> toJson(this.detailResult(head))))
              null
          }
          case Nil => ErrorCode.errorToJson("email not exist") 
          case _ => ErrorCode.errorToJson("email not exist") 
        }
    }
    
    def authCheck(token : String) : Option[(String, Int)] = {
       
        def authCheckAcc(t : String) : (String, Int) = {
            (from db() in ("user_profile") where ("user_lst.token" -> t) select (x => x)).toList match {
              case Nil => ("", authTypes.anyBody.t)
              case head :: Nil => (head.getAs[String]("user_id").get, 
                                   head.getAs[Number]("auth").map (x => x.intValue).getOrElse(authTypes.notAuth.t))
              case _ => null
            }
        }
      
        authCheckAcc(token.substring("Basic ".length())) match {
          case x : (String, Int) => Some(x)
          case null => None
        }
    }
}