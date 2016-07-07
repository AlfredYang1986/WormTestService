import play.api.GlobalSettings
import play.api.Application

import util.dao.from
import util.dao._data_connection
import util.errorcode.ErrorCode

import play.api.GlobalSettings
import module.auth.AuthModule

//import akka.actor.{Actor, Props}
//import play.api.libs.concurrent.Akka
//import play.api.templates.Html
//import play.api.libs.concurrent.Execution.Implicits.defaultContext

object Global extends GlobalSettings {
    override def onStart(application : Application) = {
        if (!_data_connection.isExisted("users")) {
            AuthModule.adminMasterCreate
        }
    }
}