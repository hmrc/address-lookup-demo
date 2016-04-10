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

import play.api.Logger
import play.api.data._
import play.api.data.Forms._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try

import services._
import uk.gov.hmrc.play.frontend.controller.FrontendController
import play.api.mvc._
import views.html.addresslookup._
import scala.concurrent.Future

object AddressLookup extends AddressLookupController with AddressLookupService with BfpoLookupService

case class AddressData(
                        nameNo: Option[String], postcode: String, hiddenselection: Option[String], noFixed: Option[String], id: Option[String],
                        editedLine1: Option[String], editedLine2: Option[String], editedLine3: Option[String], editedTown: Option[String],
                        editedCounty: Option[String]
                      )

case class IntAddData(country: Option[String], address: Option[String])

case class BFPOAddData(postcode: String)

case class BFPOEditData(postcode: String, number: Option[String], serviceNo: Option[String], rank: Option[String], name: Option[String], unitRegDep: Option[String], opName: Option[String])

trait AddressLookupController extends Controller {
  this: AddressLookupWS with BfpoLookupWS =>

  val DefaultTab = "uktab"

  import scala.concurrent.ExecutionContext.Implicits.global

  val NoErrorMessage: Option[List[AddressErrorMsg]] = None

  private def visibleTab[A](implicit request: Request[A]): String = request.getQueryString("hiddentab").getOrElse(DefaultTab)

  private def okAddr[A](errList: Option[List[AddressErrorMsg]])(implicit request: Request[A]) =
    Ok(address_lookup(addressForm, None, Countries.countries, None, errList, visibleTab))

  private def fOkAddr[A](errList: Option[List[AddressErrorMsg]])(implicit request: Request[A]) =
    Future.successful(okAddr(errList))

  def addressLookup: Action[AnyContent] = Action.async { implicit request =>
    fOkAddr(NoErrorMessage)
  }

  val BFPOAddForm = Form[BFPOAddData](
    mapping("BFPO-postcode" -> text
    )(BFPOAddData.apply)(BFPOAddData.unapply)
  )

  val BFPOEditForm = Form[BFPOEditData] {
    mapping("BFPO-postcode" -> text,
      "BFPO-number" -> optional(text),
      "BFPO-service-number" -> optional(text),
      "BFPO-rank" -> optional(text),
      "BFPO-name" -> optional(text),
      "BFPO-unit-regiment-department" -> optional(text),
      "BFPO-operation-name" -> optional(text)
    )(BFPOEditData.apply)(BFPOEditData.unapply)
  }

  def bfpoContinueButton: Action[AnyContent] = Action.async { implicit request =>
    BFPOAddForm.bindFromRequest().fold(
      formWithErrors => fOkAddr(Some(List(BlankBFPOPostcode()))),
      address => {
        if (address.postcode.nonEmpty) {
          findBfpo(address.postcode).map {
            case Right(Some(bfpo: List[BfpoDB])) =>
              Ok(address_lookup(addressForm, Some(BFPOAddTypedDetails.createInputBFPOAddress(bfpo.head)), Countries.countries, None, Some(List(EditBFPODetails())), visibleTab))
            case err =>
              okAddr(Some(List(InvalidPostcode())))
          }
        } else {
          fOkAddr(Some(List(BlankBFPOPostcode())))

        }
      }
    )
  }


  def bfpoEditButton: Action[AnyContent] = Action.async { implicit request =>
    BFPOEditForm.bindFromRequest().fold(
      formWithErrors => fOkAddr(Some(List(EditBFPODetails()))),
      address => {
        var errList: List[AddressErrorMsg] = List(EditBFPODetails())

        if (address.name.isEmpty || address.name.contains("")) errList = BFPOBlankName() :: errList
        if (address.postcode == "") errList = BFPOBlankPostcode() :: errList
        if (address.number.isEmpty || address.number.contains("")) errList = BFPOBlankNumber() :: errList
        if (address.serviceNo.isEmpty || address.serviceNo.contains("")) errList = BFPOBlankServiceNo() :: errList
        if (address.rank.isEmpty || address.rank.contains("")) errList = BFPOBlankRank() :: errList
        if (address.unitRegDep.isEmpty || address.unitRegDep.contains("")) errList = BFPOBlankUnitRegDep() :: errList

        if (errList.size == 1) {
          Future.successful(Ok(confirmationPage(None, Some(BFPOAddTypedDetails.createInputBFPOAddress(address)), None, noFixedAddress = false)))
        } else {
          Future.successful(Ok(address_lookup(addressForm, Some(BFPOAddTypedDetails.createInputBFPOAddress(address)), Countries.countries, None, Some(errList), "bfpotab")))
        }
      }
    )
  }


  val intAddForm = Form[IntAddData] {
    mapping("int-country" -> optional(text),
      "int-address" -> optional(text)
    )(IntAddData.apply)(IntAddData.unapply)
  }

  def intContinueButton: Action[AnyContent] = Action.async { implicit request =>
    intAddForm.bindFromRequest().fold(
      formWithErrors => fOkAddr(Some(List(BlankIntAddress()))),
      address => {
        if (address.address.isEmpty || address.address.contains("")) {
          fOkAddr(Some(List(BlankIntAddress())))
        } else {
          Future.successful(Ok(confirmationPage(Some(IntAddTypedDetails.createInputIntAddress(address)), None, None, noFixedAddress = false)))
        }
      }
    )
  }


  val addressForm = Form[AddressData] {
    mapping("house-name-number" -> optional(text),
      "UK-postcode" -> text.verifying("A post code is required", s => !s.isEmpty).verifying("The postcode was unrecognised", s => checkPostcodeFormat(s)),
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


  def lookupAddr(id: String, postcode: String): Future[Option[Address]] = {
    case class AddressData(nameNo: Option[String], postcode: String, hiddenselection: Option[String], noFixed: Option[String], id: Option[String], editedLine1: Option[String])

    findAddresses(postcode, None).map {
      case Right(opAddrList) => opAddrList match {
        case Some(addrLst) =>
          val matches = addrLst.filter(a => a.uprn == id)
          if (matches.nonEmpty) matches.headOption else None
        case None => None
      }
      case Left(err) => None
    }
  }

  def addressLookupSelection: Action[AnyContent] = Action.async { implicit request =>
    Logger.debug(s">>addressLookupSelection request=" + request)
    addressForm.bindFromRequest().fold(
      formWithErrors => {
        Logger.debug(s">>addressLookupSelection error=" + formWithErrors)
        Future.successful(Ok(address_lookup(formWithErrors, None, Countries.countries, None, Some(List(NoPostCode())), visibleTab)))
      },
      address => {
        Logger.debug(s">>addressLookupSelection address=" + address)
        if (address.hiddenselection.nonEmpty) editButton(address) else continueButton(address)
      }
    )
  }


  def continueButton(address: AddressData)(implicit request: Request[_]): Future[Result] = {
    Future.successful(Ok(address_lookup(addressForm.fill(address), None, Countries.countries, None, NoErrorMessage, visibleTab)))

    if (address.noFixed.contains("true")) {
      // No fixed address
      Future.successful(Ok(ukConfirmationPage(address, noFixedAddress = true)))
    } else if (address.id.nonEmpty) {
      // selected addr from list
      lookupAddr(address.id.get, address.postcode).map { addr =>
        val addrConf = addr.map(dbaddr => {
          address.copy(
            postcode = dbaddr.postcode,
            editedLine1 = Some(dbaddr.line0),
            editedLine2 = Some(dbaddr.line1),
            editedLine3 = Some(dbaddr.line2),
            editedTown = Some(dbaddr.town)
          )
        })
        Ok(ukConfirmationPage(addrConf.getOrElse(address), noFixedAddress = false))
      }
    } else if (address.editedLine1.nonEmpty) {

      Future.successful(Ok(ukConfirmationPage(address, noFixedAddress = false)))
    } else {
      Logger.debug(s">>continueButton find addr list=" + address)

      // list addresses
      findAddresses(address.postcode, address.nameNo) map {
        case Right(addressList: Option[List[Address]]) =>

          val updatedDetails: Form[AddressData] = addressList match {
            case (Some(addrLst)) =>
              Logger.debug(s">>continueButton found addr list=" + addrLst)

              addrLst.headOption.map { b =>
                addressForm.fill(AddressData(address.nameNo,
                  b.postcode,
                  address.hiddenselection,
                  address.noFixed,
                  address.id,
                  Some(b.line0),
                  Some(b.line1),
                  Some(b.line2),
                  Some(b.town),
                  address.editedCounty

                ))
              }.getOrElse(addressForm.fill(address))
            case err =>
              Logger.debug(s">>continueButton found err=" + err)
              addressForm.fill(address)
          }
          Ok(address_lookup(updatedDetails, None, Countries.countries, addressList, if (addressList.exists(_.isEmpty)) Some(List(NoMatchesFound())) else NoErrorMessage, visibleTab))
        case Left(_) =>
          Ok(address_lookup(addressForm.fill(address), None, Countries.countries, None, Some(List(NoPostCode())), visibleTab))
      }
    }
  }

  def editButton(address: AddressData)(implicit request: Request[_]): Future[Result] = {
    if (address.id.nonEmpty)
      lookupAddr(address.id.get, address.postcode).map { addr =>
        val a = addr.get
        val updatedAddr = addressForm.fill(AddressData(address.nameNo,
          a.postcode,
          address.hiddenselection,
          address.noFixed,
          address.id,
          Some(a.line0),
          Some(a.line1),
          Some(a.line2),
          Some(a.town),
          address.editedCounty

        ))
        Ok(address_lookup(updatedAddr, None, Countries.countries, None, Some(List(AddManualEntry())), visibleTab))
      }
    else Future.successful(Ok(address_lookup(addressForm, None, Countries.countries, None, Some(List(AddManualEntry())), visibleTab)))
  }


  def checkPostcodeFormat(pc: String): Boolean = {
    val r = findAddresses(pc, None) map {
      case Right(addressList) => true
      case Left(play.api.mvc.Results.BadRequest) => false
      case Left(_) => true
    }

    Await.result(r, Duration.Inf)

    r.value match {
      case Some(x: Try[Boolean]) if x.isSuccess => x.get
      case _ => false
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


object IntAddTypedDetails {
  def empty: IntAddTypedDetails = IntAddTypedDetails("", List.empty[String])

  def createInputIntAddress(addr: IntAddData): IntAddTypedDetails = IntAddTypedDetails(addr.country.getOrElse(""), addr.address.getOrElse("").split("\n").toList)
}

object BFPOAddTypedDetails {
  def empty: BFPOAddTypedDetails = BFPOAddTypedDetails("", None, None, None, None, None, None)

  def createInputBFPOAddress(bfpo: BFPOAddData): BFPOAddTypedDetails = BFPOAddTypedDetails(bfpo.postcode, None, None, None, None, None, None)

  def createInputBFPOAddress(bfpo: BFPOEditData): BFPOAddTypedDetails = BFPOAddTypedDetails(bfpo.postcode, bfpo.number, bfpo.serviceNo, bfpo.rank, bfpo.name, bfpo.unitRegDep, bfpo.opName)

  def createInputBFPOAddress(bfpoDb: BfpoDB): BFPOAddTypedDetails = BFPOAddTypedDetails(bfpoDb.postcode, Some(bfpoDb.bfpoNo), None, None, None, None, bfpoDb.opName)
}

case class BFPOAddTypedDetails(postcode: String, number: Option[String], serviceNo: Option[String], rank: Option[String], name: Option[String], unitRegDep: Option[String], opName: Option[String])

case class IntAddTypedDetails(country: String = "", address: List[String] = List.empty[String])


sealed abstract class AddressErrorMsg(msg: String)

case class BlankBFPOPostcode() extends AddressErrorMsg("Blank BFPO postcode")

case class NoPostCode() extends AddressErrorMsg("NO Postcode")

case class BlankIntAddress() extends AddressErrorMsg("Blank international address")

case class NoMatchesFound() extends AddressErrorMsg("No addresses found")

case class AddManualEntry() extends AddressErrorMsg("Manual entry")

case class InvalidPostcode() extends AddressErrorMsg("Invalid Postcode")

case class BadlyFormatedPostcode() extends AddressErrorMsg("Badly formated Postcode")

case class EditBFPODetails() extends AddressErrorMsg("Edit BFPO Details")

case class BFPOBlankName() extends AddressErrorMsg("Blank BFPO name")

case class BFPOBlankPostcode() extends AddressErrorMsg("Blank BFPO postcode")

case class BFPOBlankNumber() extends AddressErrorMsg("Blank BFPO number")

case class BFPOBlankServiceNo() extends AddressErrorMsg("Blank BFPO service number")

case class BFPOBlankRank() extends AddressErrorMsg("Blank BFPO rank")

case class BFPOBlankUnitRegDep() extends AddressErrorMsg("Blank BFPO unit/reg/dep")


trait ExampleController {
  this: Controller =>

  def index(): Action[_] = Action {
    Ok("ok")
  }
}

object ExampleController extends Controller with ExampleController

