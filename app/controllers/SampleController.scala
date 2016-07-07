package controllers

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._
import module.sample.SampleModule

object SampleController extends Controller {
    def pushSample = Action (request => requestArgs(request)(SampleModule.pushSample))
    def updateSample = Action (request => requestArgs(request)(SampleModule.updateSample))
    def pushSampleImage = Action (request => requestArgs(request)(SampleModule.pushSampleImage))
    def selectSampleImage = Action (request => requestArgs(request)(SampleModule.selectSampleImage))
    def popSampleImage = Action (request => requestArgs(request)(SampleModule.popSampleImage))
    
    def queryNotTestSample = Action (request => requestArgs(request)(SampleModule.queryNotTestSample))
    def queryTestedSample = Action (request => requestArgs(request)(SampleModule.queryTestedSample))
    def querySampleWithID = Action (request => requestArgs(request)(SampleModule.querySampleWithID))
    
    def sampleTestComplished = Action (request => requestArgs(request)(SampleModule.sampleTestComplished))
    def sampleTestPublished = Action (request => requestArgs(request)(SampleModule.sampleTestComplished))

    def samplePushTestResult = Action (request => requestArgs(request)(SampleModule.samplePushTestResult))

    def sampleConditionSearch = Action (request => requestArgs(request)(SampleModule.sampleConditionSearch))
}