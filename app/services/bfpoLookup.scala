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
import controllers.{InvalidPostcode, NoMatchesFound, AddressTypedDetails}
import play.api.http.Status._
import play.api.libs.json.{JsPath, Reads}
import play.api.libs.ws.WS
import views.html.addresslookup.address_lookup
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.functional.syntax._

import scala.concurrent.Future


trait BfpoLookupWS {
  def findBfpo(postcode: String): Future[Either[Status, Option[List[Bfpo]]]]
}

trait BfpoLookupService extends BfpoLookupWS {

  import play.api.Play.current
  import scala.concurrent.ExecutionContext.Implicits.global

  val conf1 = ConfigFactory.load()
  val lookupServer1 = conf1.getString("address-lookup-server")
 // val url = s"http://$lookupServer/address-lookup/v1/uk/addresses.json"
 val url1 = s"http://$lookupServer1/bfpo/addresses"

  implicit val bfpoReader: Reads[Bfpo] = (
    (JsPath \ "opName").readNullable[String] and
      (JsPath \\ "bfpoNo").read[String] and
      (JsPath \\ "postcode").read[String]
    ) (Bfpo.apply _)


  def findBfpo(postcode: String): Future[Either[Status, Option[List[Bfpo]]]] = {
    val query: Seq[(String, String)] = Seq(
      ("postcode", postcode))

    WS.url(url1).withHeaders("User-Agent" -> "addressLookupDemo").withQueryString(query: _*).get().map {
      case response if response.status == OK =>
        response.json match {
          case bfpos: play.api.libs.json.JsArray =>
            val bfpoList = Right(Some(bfpos.value.map { i => i.as[Bfpo] }.toList))
            bfpoList
          case err => Left(ServiceUnavailable)
        }
      case response if response.status == BAD_REQUEST =>
        Left(BadRequest)

      case _ => Left(ServiceUnavailable)
    }
  }
}


case class Bfpo(opName: Option[String], bfpoNo:String, postcode: String) {
  def toBfpoString: String = {
    s"$opName $bfpoNo $postcode"
  }
}
