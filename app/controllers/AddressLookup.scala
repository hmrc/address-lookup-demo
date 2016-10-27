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

import play.api.data.Forms._
import play.api.data._
import play.api.mvc._
import services._

//import play.filters.csrf.CSRFAddToken
import uk.gov.hmrc.play.frontend.controller.FrontendController
import views.html.addresslookup._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import scala.concurrent.Future


object AddressLookup extends AddressLookupController with AddressLookupService with BfpoLookupService

case class AddressData(
                        nameNo: Option[String], postcode: String, hiddenselection: Option[String], noFixed: Option[String], id: Option[String],
                        editedLine1: Option[String], editedLine2: Option[String], editedLine3: Option[String], editedTown: Option[String],
                        editedCounty: Option[String]
                      )

case class IntAddrData(country: String, address: String)

case class BFPOPCodeData(postcode: String)

case class BFPOEditData(postcode: String, number: String, serviceNo: String, rank: String, name: String, unitRegDep: String, opName: Option[String])

trait AddressLookupController extends FrontendController {
  this: AddressLookupWS with BfpoLookupWS =>

  import services.addresslookup._

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

  val intAddForm = Form[IntAddrData] {
    mapping("int-country" -> text.verifying("Country was left blank", _.nonEmpty),
      "int-address" -> text.verifying("Address was left blank", _.nonEmpty)
    )(IntAddrData.apply)(IntAddrData.unapply)
  }

  val BFPOAddForm = Form[BFPOPCodeData](
    mapping("BFPO-postcode" -> text.verifying("Post code was left blank", _.nonEmpty)
    )(BFPOPCodeData.apply)(BFPOPCodeData.unapply)
  )

  val BFPOEditForm = Form[BFPOEditData] {
    mapping("BFPO-postcode" -> text.verifying("A valid BFPO post code is required", _.nonEmpty),
      "BFPO-number" -> text.verifying("A valid BFPO number is required", _.nonEmpty),
      "BFPO-service-number" -> text.verifying("A valid Service number is required", _.nonEmpty),
      "BFPO-rank" -> text.verifying("Rank was left blank", _.nonEmpty),
      "BFPO-name" -> text.verifying("A valid Name is required", _.nonEmpty),
      "BFPO-unit-regiment-department" -> text.verifying("A valid Unit,Regiment and/or Department is required", _.nonEmpty),
      "BFPO-operation-name" -> optional(text)
    )(BFPOEditData.apply)(BFPOEditData.unapply)
  }


  def start: Action[AnyContent] = Action { implicit request =>
    Redirect(controllers.routes.AddressLookup.addressLookup())
  }


  def addressLookup: Action[AnyContent] = Action { implicit request =>
    Ok(address_lookup(addressForm, intAddForm, BFPOAddForm, BFPOEditForm, None, UkTab)(request, applicationMessages))
  }


  def bfpoContinueButton: Action[AnyContent] = Action.async { implicit request =>
    BFPOAddForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest(address_lookup(addressForm, intAddForm, formWithErrors, BFPOEditForm, None, BFPOTab))),
      address => {
        findBfpo(address.postcode).map {
          case Right(Some(bfpo: List[BfpoDB])) =>

            val updateForm = bfpo.headOption match {
              case Some(firstBfpo) => BFPOEditForm.fill(BFPOEditData(firstBfpo.postcode, firstBfpo.bfpoNo, "", "", "", "", firstBfpo.opName))
              case _ => BFPOEditForm
            }

            Ok(address_lookup(addressForm, intAddForm, BFPOAddForm.fill(address), updateForm, None, BFPOTab,
              Some(List(EditBFPODetails())))(request, applicationMessages))
          case err =>
            BadRequest(
              address_lookup(addressForm, intAddForm, BFPOAddForm.fill(address).withError("BFPO-postcode", "Invalid BFPO postcode found"),
                BFPOEditForm, None, BFPOTab))
        }
      }
    )
  }


  def bfpoEditButton: Action[AnyContent] = Action { implicit request =>
    BFPOEditForm.bindFromRequest().fold(
      formWithErrors => BadRequest(address_lookup(addressForm, intAddForm, BFPOAddForm, formWithErrors, None, BFPOTab,
        Some(List(EditBFPODetails())))(request, applicationMessages)),
      address => Ok(bfpoConfirmationPage(address))

    )
  }


  def intContinueButton: Action[AnyContent] = Action { implicit request =>
    intAddForm.bindFromRequest().fold(
      formWithErrors => BadRequest(address_lookup(addressForm, formWithErrors, BFPOAddForm, BFPOEditForm, None,
        IntTab)(request, applicationMessages)),
      address => Ok(intConfirmationPage(address))
    )
  }


  private def lookupAddr(id: String, postcode: String): Future[Option[Address]] = {
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
    addressForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest(address_lookup(formWithErrors, intAddForm, BFPOAddForm,
        BFPOEditForm, None, UkTab)(request, applicationMessages))),
      address => if (address.hiddenselection.nonEmpty) editButton(address) else continueButton(address)
    )
  }


  private def continueButton(address: AddressData)(implicit request: Request[_]): Future[Result] = {
    if (address.noFixed.contains("true")) {
      // No fixed address
      Future.successful(Ok(ukConfirmationPage(address, noFixedAddress = true)))
    } else if (address.id.nonEmpty) {
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
      Future.successful(
        BadRequest(address_lookup(addressForm.fill(address).withError("UK-postcode", "A post code is required"),
          intAddForm, BFPOAddForm, BFPOEditForm, None, UkTab)(request, applicationMessages)))
    } else {
      // list addresses
      findAddresses(address.postcode, address.nameNo) map {
        case Right(addressList: Option[List[Address]]) =>

          val updatedDetails: Form[AddressData] = addressList match {
            case (Some(addrLst)) =>
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
            case err => addressForm.fill(address)
          }
          Ok(address_lookup(
            updatedDetails, intAddForm, BFPOAddForm, BFPOEditForm, addressList, UkTab,
            if (addressList.exists(_.isEmpty)) Some(List(NoMatchesFound())) else None)(request, applicationMessages))
        case Left(_) =>
          BadRequest(
            address_lookup(addressForm.fill(address).withError("UK-postcode", "The postcode was unrecognised"),
              intAddForm, BFPOAddForm, BFPOEditForm, None, UkTab))
      }
    }
  }

  private def editButton(address: AddressData)(implicit request: Request[_]): Future[Result] = {
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
        Ok(address_lookup(updatedAddr, intAddForm, BFPOAddForm, BFPOEditForm, None,
          UkTab, Some(List(AddManualEntry())))(request, applicationMessages))
      }
    else Future.successful(Ok(address_lookup(addressForm, intAddForm, BFPOAddForm, BFPOEditForm, None, UkTab,
      Some(List(AddManualEntry())))(request, applicationMessages)))
  }
}

sealed abstract class OptionFlag(msg: String)

case class NoMatchesFound() extends OptionFlag("No addresses found")

case class AddManualEntry() extends OptionFlag("Manual entry")

case class EditBFPODetails() extends OptionFlag("Edit BFPO Details")

