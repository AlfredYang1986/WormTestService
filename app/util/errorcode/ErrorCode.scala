package util.errorcode

import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.libs.json.JsValue

object ErrorCode {
  	case class ErrorNode(name : String, code : Int, message : String)

  	private def xls : List[ErrorNode] = List(
  		new ErrorNode("error input", -1, "你输入的参数不正确"),
  		new ErrorNode("input company name", -2, "请输入公司名称"),
  		new ErrorNode("input legal person", -3, "请输入公司法人"),
  		new ErrorNode("input legal person id", -4, "请输入公司法人身份证"),
  		new ErrorNode("input company reg address", -5, "请输入公司注册地址"),
  		new ErrorNode("input company business", -6, "请输入公司经营范围"),
  		new ErrorNode("input company email", -7, "请输入公司联系邮箱"),
  		new ErrorNode("input driver name", -8, "请输入司机姓名"),
  		new ErrorNode("input driver secial id", -9, "请输入司机身份证号"),
  		new ErrorNode("input driver phone", -10, "请输入司机手机号"),
  		new ErrorNode("upload file error", -11, "上传照片失败"),
  		new ErrorNode("input business image", -12, "上传工商营业执照"),
  		new ErrorNode("input road image", -13, "上传运营许可证"),
  		new ErrorNode("input drive image", -14, "上传驾驶员行驶证"),
  		new ErrorNode("input drive road image", -15, "上传驾驶员驾驶证"),
  		new ErrorNode("user not exist", -16, "用户不存在或者密码不正确"),
  		new ErrorNode("wrong cell phone", -17, "输入正确的手机号"),
  		new ErrorNode("wrong code", -18, "输入正确的验证码"),
  		new ErrorNode("not validate code", -19, "验证码过期"),
  		new ErrorNode("duplicate phone or email", -20, "用户手机或邮箱已经被注册"),
  		new ErrorNode("wrong email", -21, "输入正确的邮件地址"),
  		
  		new ErrorNode("auth error", -996, "所在用户组没有访问权限"),
  		new ErrorNode("auth status error", -997, "没有通过授权"),
  		new ErrorNode("not implement", -998, "还没有实现"),
  		new ErrorNode("unknown error", -999, "unknown error")
  	)
  
  	def getErrorCodeByName(name : String) : Int = (xls.find(x => x.name == name)) match {
  			case Some(y) => y.code
  			case None => -9999
  		}
  	
   	def getErrorMessageByName(name : String) : String = (xls.find(x => x.name == name)) match {
  			case Some(y) => y.message
  			case None => "unknow error"
  		}
   	
   	def errorToJson(name : String) : JsValue =
  		Json.toJson(Map("status" -> toJson("error"), "error" -> 
  				toJson(Map("code" -> toJson(this.getErrorCodeByName(name)), "message" -> toJson(this.getErrorMessageByName(name))))))
}