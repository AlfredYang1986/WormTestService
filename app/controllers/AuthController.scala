package controllers

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._
import module.auth.AuthModule

object AuthController extends Controller {
    def register = Action (request => requestArgs(request)(AuthModule.register))
    def login = Action (request => requestArgs(request)(AuthModule.login))
    def popUser = Action(request => requestArgs(request)(AuthModule.popUser))
    def changeStatus = Action(request => requestArgs(request)(AuthModule.changeStatus))
    def changePassword = Action(request => requestArgs(request)(AuthModule.changeStatus))
        
    def lstDoctors = Action (request => requestArgs(request)(AuthModule.lstDoctors))
}