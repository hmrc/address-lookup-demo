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

import play.api.Play.current
import play.api.data.validation.Constraints._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.ws.WS
import uk.gov.hmrc.play.frontend.controller.FrontendController
import play.api.mvc._
import views.html.addresslookup._
import scala.concurrent.Future
import play.api.libs.json.Json
import play.api.libs.json._
import play.api.libs.functional.syntax._

object AddressLookup extends AddressLookup

trait AddressLookup extends FrontendController {

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

  val errorMessage = None // Some("this is an error")

  val addressLookup = Action.async { implicit request =>
    Future.successful(Ok(address_lookup(countries, None, errorMessage)))
  }

  case class AddressData(nameNo: Option[String], postcode: String, noFixed: Option[String])

  val addressForm = Form[AddressData] {
    mapping("house-name-number" -> optional(text),
            "UK-postcode" -> text,
            "no-fixed-address" -> optional(text)
      )(AddressData.apply)(AddressData.unapply)
  }


  implicit val addrReader: Reads[Address] = (
(JsPath \ "id").read[String] and
(JsPath \\ "lines").read[Array[String]] and
(JsPath \\ "town").read[String] and
(JsPath \\ "postcode").read[String]
)( Address.apply _ )


  val url = "https://www-staging.tax.service.gov.uk/address-lookup/v1/uk/addresses.json"
  val addressLookupSelection = Action.async { implicit request =>
    addressForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest),
      address => {
        if (address.postcode == "") {
          Future.successful(Ok(address_lookup(countries, None, Some("NO Postcode"))))

        } else {
          println("-------------->" + address)
          println("IM SENDING THIS POSTCODE__________" + address.postcode)
          WS.url(url).withHeaders("X-Hmrc-Origin" -> "addressLookupDemo").withQueryString("postcode" -> address.postcode, "filter" -> address.nameNo.getOrElse("")).get().map { r =>
            println("---------------->" + r.body)
            val addrs = Json.parse(r.body).asInstanceOf[play.api.libs.json.JsArray]

            val addressList: Option[List[Address]] = Some(addrs.value.map { i => i.as[Address] }.toList)
            Ok(address_lookup(countries, addressList, errorMessage))

          }
        }
      }
    )
  }
}


case class Address(uprn: String, lines: Array[String], town: String, postcode: String) {
  def toAddrString:String = {
    val lineStr = lines.mkString(" ")
    s"$lineStr $town $postcode"
  }
}


