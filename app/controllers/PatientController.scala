package controllers

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.patient.PatientModule

object PatientController extends Controller {
    def pushPatient = Action (request => requestArgs(request)(PatientModule.pushPatient))
    def updatePatient = Action (request => requestArgs(request)(PatientModule.updatePaient))
    def queryPatient = Action (request => requestArgs(request)(PatientModule.queryPatientWithId))
}