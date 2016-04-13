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
import services._
import play.api.mvc._
import play.filters.csrf.CSRFAddToken
import uk.gov.hmrc.play.frontend.controller.FrontendController
import views.html.addresslookup._

import scala.concurrent.Future

object AddressLookup extends AddressLookupController with AddressLookupService with BfpoLookupService

case class AddressData(
                        nameNo: Option[String], postcode: String, hiddenselection: Option[String], noFixed: Option[String], id: Option[String],
                        editedLine1: Option[String], editedLine2: Option[String], editedLine3: Option[String], editedTown: Option[String],
                        editedCounty: Option[String]
                      )

case class IntAddData(country: Option[String], address: String, hiddentab: String)

case class BFPOAddData(postcode: String, hiddentab: String)

case class BFPOEditData(postcode: String, number: String, serviceNo: String, rank: String, name: String, unitRegDep: String, opName: Option[String], hiddentab: String = "bfpotab")

trait AddressLookupController extends FrontendController {
  this: AddressLookupWS with BfpoLookupWS =>

  val DefaultTab = "uktab"

  import scala.concurrent.ExecutionContext.Implicits.global

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

  val intAddForm = Form[IntAddData] {
    mapping("int-country" -> optional(text),
      "int-address" -> text.verifying("Address was left blank", _.length > 0),
      "hiddentab" -> default(text, "inttab")
    )(IntAddData.apply)(IntAddData.unapply)
  }

  val BFPOAddForm = Form[BFPOAddData](
    mapping("BFPO-postcode" -> text.verifying("Post code was left blank", _.length > 0),
      "hiddentab" -> default(text, "bfpotab")
    )(BFPOAddData.apply)(BFPOAddData.unapply)
  )

  val BFPOEditForm = Form[BFPOEditData] {
    mapping("BFPO-postcode" -> text.verifying("A valid BFPO post code is required", _.length > 0),
      "BFPO-number" -> text.verifying("A valid BFPO number is required", _.length > 0),
      "BFPO-service-number" -> text.verifying("A valid Service number is required", _.length > 0),
      "BFPO-rank" -> text.verifying("Rank was left blank", _.length > 0),
      "BFPO-name" -> text.verifying("A valid Name is required", _.length > 0),
      "BFPO-unit-regiment-department" -> text.verifying("A valid Unit,Regiment and/or Department is required", _.length > 0),
      "BFPO-operation-name" -> optional(text),
      "hiddentab" -> default(text, "bfpotab")
    )(BFPOEditData.apply)(BFPOEditData.unapply)
  }



  def start: Action[AnyContent] = CSRFAddToken {Action { implicit request =>
    Redirect(controllers.routes.AddressLookup.addressLookup)
  }}


  def addressLookup: Action[AnyContent] = CSRFAddToken{ Action { implicit request =>
    Ok(address_lookup(addressForm, intAddForm, BFPOAddForm, BFPOEditForm, None, None, "uktab"))
  }}

  def bfpoContinueButton: Action[AnyContent] = Action.async { implicit request =>
    BFPOAddForm.bindFromRequest().fold(
      formWithErrors => {
        Future.successful(BadRequest(address_lookup(addressForm, intAddForm, formWithErrors, BFPOEditForm, None, None, "bfpotab")))
      },
      address => {
        findBfpo(address.postcode).map {
          case Right(Some(bfpo: List[BfpoDB])) =>

            val updateForm = bfpo.headOption match {
              case Some(firstBfpo) => BFPOEditForm.fill(BFPOEditData(firstBfpo.postcode, firstBfpo.bfpoNo, "", "", "", "", firstBfpo.opName))
              case _ => BFPOEditForm
            }

            Ok(address_lookup(addressForm, intAddForm, BFPOAddForm.fill(address), updateForm, None, Some(List(EditBFPODetails())), "bfpotab"))
          case err =>
            BadRequest(address_lookup(addressForm, intAddForm, BFPOAddForm.fill(address).withError("BFPO-postcode", "Invalid BFPO postcode found"), BFPOEditForm, None, None, "bfpotab"))
        }
      }
    )
  }


  def bfpoEditButton: Action[AnyContent] = Action { implicit request =>
    BFPOEditForm.bindFromRequest().fold(
      formWithErrors => BadRequest(address_lookup(addressForm, intAddForm, BFPOAddForm, formWithErrors, None, Some(List(EditBFPODetails())), "bfpotab")),
      address => Ok(bfpoConfirmationPage(address))

    )
  }


  def intContinueButton: Action[AnyContent] = Action { implicit request =>
    intAddForm.bindFromRequest().fold(
      formWithErrors => BadRequest(address_lookup(addressForm, formWithErrors, BFPOAddForm, BFPOEditForm, None, None, "inttab")),
      address => Ok(intConfirmationPage(address))
    )
  }


  def lookupAddr(id: String, postcode: String): Future[Option[Address]] = {
    //    case class AddressData(nameNo: Option[String], postcode: String, hiddenselection: Option[String], noFixed: Option[String], id: Option[String], editedLine1: Option[String])

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

  def addressLookupSelection: Action[AnyContent] = CSRFAddToken {Action.async { implicit request =>
    addressForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest(address_lookup(formWithErrors, intAddForm, BFPOAddForm, BFPOEditForm, None, None, "uktab"))),
      address => if (address.hiddenselection.nonEmpty) editButton(address) else continueButton(address)
    )
  }}


  def continueButton(address: AddressData)(implicit request: Request[_]): Future[Result] = {
    Future.successful(Ok(address_lookup(addressForm.fill(address), intAddForm, BFPOAddForm, BFPOEditForm, None, None, "uktab")))
    if (address.noFixed.contains("true")) {
      // No fixed address
      Future.successful(Ok(ukConfirmationPage(address, noFixedAddress = true)))
    } else if (address.id.nonEmpty) {
      // has an item in a list been selected?
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
    } else if (address.postcode.isEmpty) {
      Future.successful(BadRequest(address_lookup(addressForm.fill(address).withError("UK-postcode", "A post code is required"), intAddForm, BFPOAddForm, BFPOEditForm, None, None, "uktab")))

    } else {
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
          Ok(address_lookup(updatedDetails, intAddForm, BFPOAddForm, BFPOEditForm, addressList, if (addressList.exists(_.isEmpty)) Some(List(NoMatchesFound())) else None, "uktab"))
        case Left(_) =>
          BadRequest(address_lookup(addressForm.fill(address).withError("UK-postcode", "The postcode was unrecognised"), intAddForm, BFPOAddForm, BFPOEditForm, None, None, "uktab"))
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
        Ok(address_lookup(updatedAddr, intAddForm, BFPOAddForm, BFPOEditForm, None, Some(List(AddManualEntry())), "uktab"))
      }
    else Future.successful(Ok(address_lookup(addressForm, intAddForm, BFPOAddForm, BFPOEditForm, None, Some(List(AddManualEntry())), "uktab")))
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



sealed abstract class OptionFlag(msg: String)

//case class BlankIntAddress() extends OptionFlag("Blank international address")

case class NoMatchesFound() extends OptionFlag("No addresses found")

case class AddManualEntry() extends OptionFlag("Manual entry")

case class EditBFPODetails() extends OptionFlag("Edit BFPO Details")

