/*
 * Copyright 2016 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers

import com.typesafe.config.ConfigFactory
import play.api.data._
import play.api.data.Forms._
import play.api.mvc.Results._
import services.{Address, AddressLookupWS, AddressLookupService}
import uk.gov.hmrc.play.frontend.controller.FrontendController
import play.api.mvc._
import views.html.addresslookup._
import scala.concurrent.Future

object AddressLookup extends AddressLookupController  with AddressLookupService

case class AddressData(nameNo: Option[String], postcode: String, hiddenselection: Option[String], noFixed: Option[String], id: Option[String], editedLine1: Option[String], editedLine2: Option[String], editedLine3: Option[String], editedTown: Option[String], editedCounty: Option[String])

trait AddressLookupController extends FrontendController {
  this: AddressLookupWS =>

  import scala.concurrent.ExecutionContext.Implicits.global

  val NoErrorMessage: Option[List[AddressErrorMsg]] = None

  val addressLookup: Action[AnyContent] = Action.async { implicit request =>
    Future.successful(Ok(address_lookup(AddressTypedDetails.empty, None, Countries.countries, None, NoErrorMessage)))
  }


  val addressForm = Form[AddressData] {
    mapping("house-name-number" -> optional(text),
      "UK-postcode" -> text,
      "hiddenselection" -> optional(text),
      "no-fixed-address" -> optional(text),
      "radio-inline-group" -> optional(text),
      "UK-address-line1" -> optional(text),
      "UK-address-line2" -> optional(text),
      "UK-address-line3" -> optional(text),
      "UK-town" -> optional(text),
      "UK-county" -> optional(text)
    )(AddressData.apply)(AddressData.unapply)
  }



  def lookupAddr(id: String, postcode:String): Future[Option[Address]] = {
    case class AddressData(nameNo: Option[String], postcode: String, hiddenselection: Option[String], noFixed: Option[String], id: Option[String], editedLine1: Option[String])

    findAddresses(postcode, None).map{
      case Right(opAddrList) => opAddrList match {
        case Some(addrLst) =>
          val matches = addrLst.filter( a => a.uprn == id)
          if( matches.nonEmpty) matches.headOption else None
        case None => None
      }
      case Left(err) => None
    }

  }

  val addressLookupSelection: Action[AnyContent] = Action.async { implicit request =>
    addressForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest),
      address => {
        if (address.hiddenselection.nonEmpty) editButton(address) else continueButton(address)
      }
    )
  }


  def continueButton(address:AddressData)(implicit request:Request[_]):Future[Result] = {
    if (address.noFixed.contains("true")){
      Future.successful(Ok(confirmationPage(AddressTypedDetails.empty, None, true)))
    } else if (address.id.nonEmpty) {
      lookupAddr(address.id.get, address.postcode).map{ addr =>
        Ok(confirmationPage(AddressTypedDetails.empty, addr, false))
      }
    } else if (address.editedLine1.nonEmpty) {
      Future.successful(Ok(confirmationPage(AddressTypedDetails.createFromInputAddress(address), None, false)))
    } else {
      if (address.postcode == "") {
        Future.successful(Ok(address_lookup(AddressTypedDetails.empty, None, Countries.countries, None, Some(List(NoPostCode())))))
      } else {
        findAddresses(address.postcode, address.nameNo) map {
          case Right(addressList) =>
            Ok(address_lookup(AddressTypedDetails.createFromInputAddress(address), None, Countries.countries, addressList, if (addressList.exists(_.isEmpty)) Some(List(NoMatchesFound())) else NoErrorMessage))
          case Left(play.api.mvc.Results.BadRequest) => Ok(address_lookup(AddressTypedDetails.empty.copy(postcode = address.postcode), None, Countries.countries, None, Some(List(BadlyFormatedPostcode()))))
          case Left(_) => Ok(address_lookup(AddressTypedDetails(address.postcode), None, Countries.countries, None, Some(List(NoPostCode()))))
        }
      }
    }
  }

  def editButton(address:AddressData)(implicit request:Request[_]): Future[Result] =  {
    if (address.id.nonEmpty) {
      lookupAddr(address.id.get, address.postcode).map{ addr =>
        Ok(address_lookup(AddressTypedDetails.createFromInputAddress(address), addr, Countries.countries, None, Some(List(AddManualEntry()))))
      }
    } else {
      Future.successful(Ok(address_lookup(AddressTypedDetails.createFromInputAddress(address), None, Countries.countries, None, Some(List(AddManualEntry())))))
    }
  }
}


object Countries {
  val countries = List[String](
    "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda",
    "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh",
    "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia",
    "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burma",
    "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Republic", "Chad",
    "Chile", "China", "Colombia", "Comoros", "Congo, Democratic Republic", "Congo, Republic of the",
    "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti",
    "Dominica", "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador",
    "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France", "Gabon",
    "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Greenland", "Grenada", "Guatemala", "Guinea",
    "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong", "Hungary", "Iceland", "India",
    "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan",
    "Kazakhstan", "Kenya", "Kiribati", "Korea, North", "Korea, South", "Kuwait", "Kyrgyzstan", "Laos",
    "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg",
    "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands",
    "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Mongolia", "Morocco", "Monaco",
    "Mozambique", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger",
    "Nigeria", "Norway", "Oman", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "Peru",
    "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Samoa", "San Marino",
    "Sao Tome", "Saudi Arabia", "Senegal", "Serbia and Montenegro", "Seychelles", "Sierra Leone",
    "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "Spain",
    "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syria", "Taiwan",
    "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey",
    "Turkmenistan", "Uganda", "Ukraine", "United Arab Emirates", "United States",
    "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"
  )

}


object AddressTypedDetails {
  def empty: AddressTypedDetails = AddressTypedDetails("", "", "", "", "", "", "")

  def formatPostcode(pc:String):String = {
    val upc = pc.toUpperCase
    if (!upc.contains(" ")){
      val (fst, sst) = upc.splitAt(upc.length - 3)
      fst + " " + sst
    } else upc
  }

  def createFromInputAddress(addr:AddressData): AddressTypedDetails = AddressTypedDetails(formatPostcode(addr.postcode), addr.nameNo.getOrElse(""), addr.editedLine1.getOrElse(""), addr.editedLine2.getOrElse(""), addr.editedLine3.getOrElse(""), addr.editedTown.getOrElse(""), addr.editedCounty.getOrElse(""))
}

case class AddressTypedDetails(postcode: String, flatNumber: String = "", line1: String = "", line2: String = "", line3: String = "", town: String = "", county: String = "")

sealed abstract class AddressErrorMsg(msg: String)

case class NoPostCode() extends AddressErrorMsg("NO Postcode")

case class NoMatchesFound() extends AddressErrorMsg("No addresses found")

case class AddManualEntry() extends AddressErrorMsg("Manual entry")

case class InvalidPostcode() extends AddressErrorMsg("Invalid Postcode")

case class BadlyFormatedPostcode() extends AddressErrorMsg("Badly formated Postcode")


trait ExampleController {
  this: Controller =>

  def index(): Action[_] = Action {
    Ok("ok")
  }
}

object ExampleController extends Controller with ExampleController