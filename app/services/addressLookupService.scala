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

package services

import com.typesafe.config.ConfigFactory
import play.api.http.Status._
import play.api.libs.json.{JsPath, Reads}
import play.api.libs.ws.WS
import play.api.mvc.Results._
import play.api.libs.functional.syntax._

import scala.concurrent.Future


trait AddressLookupWS {
  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[Address]]]]
}

trait AddressLookupService extends AddressLookupWS {

  import play.api.Play.current
  import scala.concurrent.ExecutionContext.Implicits.global

  private lazy val conf = ConfigFactory.load()
  private lazy val lookupServer = conf.getString("address-lookup-server")
  private lazy val url = s"http://$lookupServer/uk/addresses"


  implicit val addrReader: Reads[Address] = (
    (JsPath \ "id").read[String] and
      (JsPath \\ "lines").read[Array[String]] and
      (JsPath \\ "town").read[String] and
      (JsPath \\ "postcode").read[String]
    ) (Address.apply _)


  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[Address]]]] = {
    val query: Seq[(String, String)] = Seq(("postcode", postcode)) ++ filter.map(name => "filter" -> name)

    WS.url(url).withHeaders("User-Agent" -> "addressLookupDemo").withQueryString(query: _*).get().map {
      case response if response.status == OK =>
        response.json match {
          case addrs: play.api.libs.json.JsArray =>
            val addressList = Right(Some(addrs.value.map { i => i.as[Address] }.toList))
            addressList
          case err =>
            Left(ServiceUnavailable)
        }
      case response if response.status == BAD_REQUEST =>
        Left(BadRequest)

      case err =>
        Left(ServiceUnavailable)
    }
  }
}


case class Address(uprn: String, lines: Array[String], town: String, postcode: String) {
  def toAddrString: String = {
    val lineStr = lines.mkString(", ")
    s"$lineStr, $town, $postcode"
  }

  def line0 = if (lines.length < 1) "" else lines(0)

  def line1 = if (lines.length < 2) "" else lines(1)

  def line2 = if (lines.length < 3) "" else lines(2)
}
