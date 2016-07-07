package module.common.xml

import scala.xml._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue

object xmlOpt {
    lazy val doc_cities = xml.XML.loadFile("resource/city_defines.xml")
    
    val allCities : List[JsValue] =
        ((doc_cities \ "province") map (x => 
            toJson(Map("province" -> toJson((x \ "@name").text),
                       "cities" -> toJson((x \ "city" \\ "@name") map (y => y.text)))))).toList
}