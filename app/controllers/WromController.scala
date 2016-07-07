package controllers

import play.api._
import play.api.mvc._

import controllers.common.requestArgsQuery._
import module.auth.AuthModule
import module.worm.WormModule

object WromController extends Controller {
    def wormCategories = Action (request => requestArgs(request)(WormModule.wormGroupByCategories))
    def onlyCategories = Action (request => requestArgs(request)(WormModule.wormCategories))
    def pushWormCategory = Action (request => requestArgs(request)(WormModule.pushWormCategory)) 
    def popWormCategory = Action (request => requestArgs(request)(WormModule.popWormCategory)) 
   
    def pushWorm = Action (request => requestArgs(request)(WormModule.pushWorm))
    def popWorm = Action (request => requestArgs(request)(WormModule.popWorm))
    def updateDescription = Action (request => requestArgs(request)(WormModule.updateWormDescription))
    def queryWorm = Action (request => requestArgs(request)(WormModule.queryWorm))
    def queryWormImages = Action (request => requestArgs(request)(WormModule.queryWromImages))
    def wormSetting = Action (request => requestArgs(request)(WormModule.wormSetting))
    def wormSettingQuery = Action (request => requestArgs(request)(WormModule.wormSettingQuery))
    
    def pushWormImage = Action (request => requestArgs(request)(WormModule.pushWormImage))
    def popWormImage = Action (request => requestArgs(request)(WormModule.popWormImage))
}