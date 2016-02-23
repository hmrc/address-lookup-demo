package controllers

import uk.gov.hmrc.play.frontend.controller.FrontendController
import play.api.mvc._
import views.html.addresslookup._
import scala.concurrent.Future

object AddressLookup extends AddressLookup

trait AddressLookup extends  FrontendController {
  val addressLookup = Action.async { implicit request =>
    Future.successful(Ok(address_lookup()))
  }


}