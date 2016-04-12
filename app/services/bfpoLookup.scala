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
import views.html.addresslookup.address_lookup
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.functional.syntax._

import scala.concurrent.Future


trait BfpoLookupWS {
  def findBfpo(postcode: String): Future[Either[Status, Option[List[BfpoDB]]]]
}

trait BfpoLookupService extends BfpoLookupWS {

  import play.api.Play.current
  import scala.concurrent.ExecutionContext.Implicits.global

  private lazy val conf = ConfigFactory.load()
  private lazy val lookupServer1 = conf.getString("address-lookup-server")
  private lazy val url = s"http://$lookupServer1/bfpo/addresses"

  implicit val bfpoReader: Reads[BfpoDB] = (
    (JsPath \ "opName").readNullable[String] and
      (JsPath \\ "bfpoNo").read[String] and
      (JsPath \\ "postcode").read[String]
    ) (BfpoDB.apply _)


  def findBfpo(postcode: String): Future[Either[Status, Option[List[BfpoDB]]]] = {
    val query: Seq[(String, String)] = Seq(("postcode", postcode))

    WS.url(url).withHeaders("User-Agent" -> "addressLookupDemo").withQueryString(query: _*).get().map {
      case response if response.status == OK =>
        response.json match {
          case bfpos: play.api.libs.json.JsArray =>
            val bfpoList = Right(Some(bfpos.value.map { i => i.as[BfpoDB] }.toList))
            bfpoList
          case err => Left(ServiceUnavailable)
        }
      case response if response.status == BAD_REQUEST =>
        Left(BadRequest)

      case _ => Left(ServiceUnavailable)
    }
  }
}


case class BfpoDB(opName: Option[String], bfpoNo:String, postcode: String)
