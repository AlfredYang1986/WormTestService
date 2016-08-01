package module.worm

import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode
import com.mongodb.casbah.Imports._

object WormModule {
  
    def categoriesIsExist(cat : String) : Boolean = 
        (from db() in "worm_cat" where ("name" -> cat) select (x => x)).toList match {
          case Nil => false
          case head :: Nil => true
          case _ => false
        }
  
    def wormCategories(data : JsValue) : JsValue = 
        toJson(Map("status" -> toJson("ok"), "method" -> toJson("onlyCategories"), "result" -> toJson( 
            (from db() in "worm_cat" select (x => x.getAs[String]("name").get)).toList)))
            
    def wormGroupByCategories(data : JsValue) : JsValue = 
        toJson(Map("status" -> toJson("ok"), "method" -> toJson("wormCategories"), "result" -> toJson(
            (from db() in "worms" select (wormDetail2JsValue(_))).toList.groupBy (x => (x \ "cat").asOpt[String].get).
            asInstanceOf[Map[String, List[JsValue]]])))
            
    def pushWormCategory(data : JsValue) : JsValue = {
        val cat = (data \ "cat").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
        if (!categoriesIsExist(cat)) {
            val builder = MongoDBObject.newBuilder
            builder += "name" -> cat
            _data_connection.getCollection("worm_cat") += builder.result
        }
        
        toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushWormCategory"), "result" -> toJson("push cat success")))
    }
    
    def popWormCategory(data : JsValue) : JsValue = {
        val cat = (data \ "cat").asOpt[String].map (x => x).getOrElse(throw new Exception("wrong input"))
        if (categoriesIsExist(cat)) {
            _data_connection.getCollection("worm_cat") -= DBObject("name" -> cat)
        }
        
        toJson(Map("status" -> toJson("ok"), "method" -> toJson("popWormCategory"), "result" -> toJson("push cat success")))
    }
    
    def Js2DBObject(data : JsValue) : Option[MongoDBObject] = 
        try {
            val builder = MongoDBObject.newBuilder
          
            (data \ "name").asOpt[String].map (x => builder += "name" -> x).getOrElse(throw new Exception("need name"))
            (data \ "cat").asOpt[String].map (x => builder += "cat" -> x).getOrElse(throw new Exception("need cat"))
            
            (data \ "description").asOpt[String].map (x => builder += "description" -> x).getOrElse(builder += "description" -> "")
            
            val img_lst = MongoDBList.newBuilder
            (data \ "images").asOpt[List[String]].map { x => x.map { iter => 
               img_lst += iter
            }}.getOrElse(Unit)
            builder += "img_lst" -> img_lst.result
            
            builder += "setting" -> 1.asInstanceOf[Int]
    
            Some(builder.result)
        } catch {
          case ex : Exception => None
        }
        
    def wormLst2JsValue(obj : MongoDBObject) : JsValue = 
        toJson(Map("name" -> toJson(obj.getAs[String]("name").get),
                   "cat" -> toJson(obj.getAs[String]("cat").get)))
    
    def wormDetail2JsValue(obj : MongoDBObject) : JsValue = 
        toJson(Map("name" -> toJson(obj.getAs[String]("name").get),
                   "cat" -> toJson(obj.getAs[String]("cat").get),
                   "setting" -> toJson(obj.getAs[Number]("setting").get.intValue),
                   "img_lst" -> toJson(obj.getAs[MongoDBList]("img_lst").get.toList.asInstanceOf[List[String]]),
                   "description" -> toJson(obj.getAs[String]("description").get)))
    
    def pushWorm(data : JsValue) : JsValue = 
        try {
            Js2DBObject(data) match {
              case None => throw new Exception("error")
              case Some(x) => {
                  _data_connection.getCollection("worms") += x
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushWorm"), 
                             "result" -> toJson(Map("name" -> toJson((data \ "name").asOpt[String].get),
                                                    "cat" -> toJson((data \ "cat").asOpt[String].get)))))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def popWorm(data : JsValue) : JsValue =
        try {
            val name = (data \ "name").asOpt[String].map (x => x).getOrElse(throw new Exception("need name"))
            val cat = (data \ "cat").asOpt[String].map (x => x).getOrElse(throw new Exception("need cat"))
            
            (from db() in "worms" where ("name" -> name, "cat" -> cat) select (x => x)).toList match {
              case Nil => throw new Exception("not exist")
              case head :: Nil => {
                  _data_connection.getCollection("worms") -= head
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("popWorm"), "result" -> toJson((data \ "name").asOpt[String].get)))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def queryWorm(data : JsValue) : JsValue =
        try {
            val cat = (data \ "cat").asOpt[String].map (x => x).getOrElse(throw new Exception("need name"))
            val name = (data \ "name").asOpt[String].map (x => x).getOrElse(throw new Exception("need name"))
            toJson(Map("status" -> toJson("ok"), "method" -> toJson("queryWorm"), "result" -> toJson(
                    (from db() in "worms" where ("cat" -> cat, "name" -> name) select (wormDetail2JsValue(_))).toList.head)))
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    
    def queryWromImages(data : JsValue) : JsValue =
        try {
            val name = (data \ "name").asOpt[String].map (x => x).getOrElse(throw new Exception("need name"))
            (from db() in "worms" where ("name" -> name) select (x => x)).toList match {
              case Nil => throw new Exception("not exist")
              case head :: Nil => {
                  val img_lst = head.getAs[MongoDBList]("images").get
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson(img_lst.asInstanceOf[List[String]])))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
     
    def wormSetting(data : JsValue) : JsValue = 
        try {
            val cat = (data \ "cat").asOpt[String].map (x => x).getOrElse(throw new Exception("need name"))
            val name = (data \ "name").asOpt[String].map (x => x).getOrElse(throw new Exception("need name"))
            val setting = (data \ "setting").asOpt[Int].map (x => x).getOrElse(throw new Exception("need name"))
            (from db() in "worms" where ("name" -> name, "cat" -> cat) select (x => x)).toList match {
              case Nil => throw new Exception("not exist")
              case head :: Nil => {
                  head += "setting" -> setting.asInstanceOf[Number]
                  _data_connection.getCollection("worms").update(DBObject("cat"-> cat, "name" -> name), head)
                  toJson(Map("status" -> toJson("ok"), "result" -> toJson("success")))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        } 
        
    def wormSettingQuery(data : JsValue) : JsValue = 
        toJson(Map("status" -> toJson("ok"), "method" -> toJson("wormSettingQuery"), "result" -> toJson(
            (from db() in "worms" where ("setting" -> 1) select (wormDetail2JsValue(_))).toList.groupBy (x => (x \ "cat").asOpt[String].get).
            asInstanceOf[Map[String, List[JsValue]]])))
            
    def updateWormDescription(data : JsValue) : JsValue =    
        try {
            val worm_name = (data \ "name").asOpt[String].map (x => x).getOrElse(throw new Exception("name is needed"))
            val des = (data \ "description").asOpt[String].map (x => x).getOrElse(throw new Exception("des is needed"))
            
            (from db() in "worms" where ("name" -> worm_name) select (x => x)).toList match {
              case Nil => throw new Exception("not existing")
              case head :: Nil => {
                  head += "description" -> des
                  _data_connection.getCollection("worms").update(DBObject("name" -> worm_name), head)
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("updateWormDescription"), "result" -> toJson("update success")))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
        
    def pushWormImage(data : JsValue) : JsValue = {
        try {
            val worm_name = (data \ "name").asOpt[String].map (x => x).getOrElse(throw new Exception("name is needed"))
            val image = (data \ "image").asOpt[String].map (x => x).getOrElse(throw new Exception("image is needed"))
            
            (from db() in "worms" where ("name" -> worm_name) select (x => x)).toList match {
              case Nil => throw new Exception("not existing")
              case head :: Nil => {
                  val lst = head.getAs[MongoDBList]("img_lst").get
                  lst += image
                  _data_connection.getCollection("worms").update(DBObject("name" -> worm_name), head)
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("pushWormImage"), "result" -> toJson(Map("name" -> worm_name, "image" -> image))))
              }
            }
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def popWormImage(data : JsValue) : JsValue = {
        try {
            val worm_name = (data \ "sample_id").asOpt[String].map (x => x).getOrElse(throw new Exception("sample not exist"))
            val image = (data \ "image").asOpt[String].map (x => x).getOrElse(throw new Exception("image not set"))
            
            (from db() in "worms" where ("name" -> worm_name) select (x => x)).toList match {
              case Nil => throw new Exception("sample not exist")
              case head :: Nil => {
                  var lst = head.getAs[MongoDBList]("img_lst").get
                  head += "images" -> lst.filterNot(x => image.equals(x.asInstanceOf[String])).toList
                  _data_connection.getCollection("worms").update(DBObject("name" -> worm_name), head)
                  toJson(Map("status" -> toJson("ok"), "method" -> toJson("popWormImage"), "result" -> toJson(Map("name" -> worm_name, "image" -> image))))
              }
            }
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
    
    def wormSourceImport(data : JsValue) : JsValue = {
        try {
            if (_data_connection.isExisted("worm_cat")) {
                _data_connection.getCollection("worm_cat").drop()
            }
            
            if (_data_connection.isExisted("worms")) {
                _data_connection.getCollection("worms").drop()
            }
            
            toJson(Map("status" -> toJson("ok"), "method" -> toJson("wormSourceImport"), "result" -> toJson("success")))
          
        } catch {
          case ex : Exception => ErrorCode.errorToJson(ex.getMessage)
        }
    }
}