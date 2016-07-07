package controllers.common

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import play.api.libs.Files.TemporaryFile

import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._
//import module.auth.AuthModule.authCheckUser

object requestArgsQuery extends Controller {
  def requestArgs(request : Request[AnyContent])(func : JsValue => JsValue) : Result = {
  		try {
  			request.body.asJson.map { x => 
  			    Ok(func(x))
  			}.getOrElse (BadRequest("Bad Request for input"))
  		} catch {
  			case _ : Exception => BadRequest("Bad Request for input")
  		}  		   
	}
  
  	def requestGetRequestArgs(request :Request[AnyContent])(auth : String => Option[(String, Int)])(func : (String, JsValue) => JsValue)(basicStatus : Int)(basicAuth : Int) : Result = {
  	   try {
  	     request.body.asJson.map { x => 
  	         request.headers.get("Authorization").map (auth(_)).getOrElse(None) match {
  	           case Some((user_id, auth)) => { 
//  	                if (status < basicStatus) Ok(ErrorCode.errorToJson("auth status error"))
  	                if (auth < basicAuth) Ok(ErrorCode.errorToJson("auth error"))
  	                else Ok(func(user_id, x))
  	           }
  	           case None => Ok(ErrorCode.errorToJson("email not exist"))
  	         }
  	     }.getOrElse(BadRequest)
  	   } catch {
  	     case _ : Exception => BadRequest
  	   }
  	}
  	
  	def uploadRequestArgs(request : Request[AnyContent])(func : MultipartFormData[TemporaryFile] => JsValue) : Result = {
  		try {
   			request.body.asMultipartFormData.map { x => 
   				Ok(func(x))
  			}.getOrElse (BadRequest("Bad Request for input")) 			  
  		} catch {
  			case _ : Exception => BadRequest("Bad Request for input")
  		}
  	}
}