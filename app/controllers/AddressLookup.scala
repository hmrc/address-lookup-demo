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
import services.{AddressLookupService, Address}
import uk.gov.hmrc.play.frontend.controller.FrontendController
import play.api.mvc._
import views.html.addresslookup._
import scala.concurrent.Future

object AddressLookup extends AddressLookup

trait AddressLookup extends FrontendController  {

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

  val NoErrorMessage:Option[List[AddressErrorMsg]] = None // Some("this is an error")

  val addressLookup = Action.async { implicit request =>
    Future.successful(Ok(address_lookup(AddressTypedDetails.empty, countries, None, NoErrorMessage)))
  }

  case class AddressData(nameNo: Option[String], postcode: String, noFixed: Option[String])

  val addressForm = Form[AddressData] {
    mapping("house-name-number" -> optional(text),
            "UK-postcode" -> text,
            "no-fixed-address" -> optional(text)
      )(AddressData.apply)(AddressData.unapply)
  }


//  val conf = ConfigFactory.load()
//  val lookupServer = conf.getString("address-lookup-server")
//
//  val url = s"https://$lookupServer/address-lookup/v1/uk/addresses.json"


  val addressLookupSelection = Action.async { implicit request =>
    addressForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest),
      address => {
        if (address.postcode == "") {
          Future.successful(Ok(address_lookup(AddressTypedDetails(address.postcode), countries, None, Some(List(NoPostCode())))))
        } else {
          AddressLookupService.findAddresses(address.postcode, address.nameNo) map {
            case Right(addressList) =>
              Ok(address_lookup(AddressTypedDetails(address.postcode, address.nameNo.getOrElse("")), countries, addressList, if (addressList.exists(_.isEmpty)) Some(List(NoMatchesFound())) else NoErrorMessage))
            case Left(err) => err
          }
        }
      }
    )
  }


  val addressLookupEdit = Action.async { implicit request =>
    Future.successful( Ok(address_lookup(AddressTypedDetails.empty, countries, None,  Some(List(AddManualEntry())) ) ))
  }
}




object AddressTypedDetails {
  def empty = AddressTypedDetails("", "")
}

case class AddressTypedDetails( postcode:String, flatNumber:String = "", line1:String = "", line2:String = "", town:String = "", county:String = "" )


sealed abstract class AddressErrorMsg(msg:String)
case class NoPostCode() extends AddressErrorMsg("NO Postcode")
case class NoMatchesFound() extends AddressErrorMsg("No addresses found")
case class AddManualEntry() extends AddressErrorMsg("Manual entry")


