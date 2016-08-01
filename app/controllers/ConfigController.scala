package controllers

import play.api._
import play.api.mvc._
import controllers.common.requestArgsQuery._

import module.paraconfig.ConfigModule

object ConfigController extends Controller {
    def pushResourceType = Action (request => requestArgs(request)(ConfigModule.pushResourceType))
    def popResourceType = Action (request => requestArgs(request)(ConfigModule.popResourceType))
    def queryResourceType = Action (request => requestArgs(request)(ConfigModule.queryResourceType))
    
    def pushPatientType = Action (request => requestArgs(request)(ConfigModule.pushPatientType))
    def popPatientType = Action (request => requestArgs(request)(ConfigModule.popPatientType))
    def queryPatientType = Action (request => requestArgs(request)(ConfigModule.queryPatientType))
}