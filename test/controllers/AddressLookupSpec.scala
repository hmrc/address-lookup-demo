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


import play.api.mvc.Results._
import services.{AddressLookupWS, BfpoDB, BfpoLookupWS}

import concurrent._
import play.api.mvc._
import play.api.test._
import org.scalatestplus.play._
import play.api.data.Form
import play.api.data.Forms._
import play.api.test.Helpers._
import play.filters.csrf.CSRF
import services.addresslookup.UkTab

class AddressLookupSpec extends PlaySpec with Results with OneAppPerSuite {

  "addressLookup action" should {

    "check address template" in {
      // new WithApplication
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
        mapping("int-country" -> optional(text),
          "int-address" -> text.verifying("Address was left blank", _.length > 0)
        )(IntAddrData.apply)(IntAddrData.unapply)
      }

      val BFPOAddForm = Form[BFPOPCodeData](
        mapping("BFPO-postcode" -> text.verifying( "Post code was left blank", _.length > 0)
        )(BFPOPCodeData.apply)(BFPOPCodeData.unapply)
      )

      val BFPOEditForm = Form[BFPOEditData] {
        mapping("BFPO-postcode" -> text.verifying( "A valid BFPO post code is required", _.length > 0),
          "BFPO-number" -> text.verifying( "A valid BFPO number is required", _.length > 0),
          "BFPO-service-number" -> text.verifying( "A valid Service number is required", _.length > 0),
          "BFPO-rank" -> text.verifying( "Rank was left blank", _.length > 0),
          "BFPO-name" -> text.verifying( "A valid Name is required", _.length > 0),
          "BFPO-unit-regiment-department" -> text.verifying( "A valid Unit,Regiment and/or Department is required", _.length > 0),
          "BFPO-operation-name" -> optional(text)
        )(BFPOEditData.apply)(BFPOEditData.unapply)
      }


      val html = views.html.addresslookup.address_lookup(addressForm, intAddForm, BFPOAddForm,BFPOEditForm, None, UkTab)(
        FakeRequest().withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken))
      contentAsString(html) must include("Your Address")
    }

    "return a blank page" in {
      val controller = new AddressLookupController with DummyWS
      val request = FakeRequest().withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result = controller.addressLookup().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("Your Address")
    }
  }

  "addressLookupSelection action" should {
    "check if 'edit this addr' is selected we return edit fields" in {
      val controller = new AddressLookupController with DataWS
      val request = FakeRequest(GET,
        "/address-lookup-demo/address-lookup-selection?hiddenselection=hiddenselection&radio-inline-group=GB00001&UK-postcode=AA1AA1"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("Address line 1")
      bodyText must include("Address line 2")
      bodyText must include("Nearest town or city")
      bodyText must include("County")
    }

    "check if 'continue' is selected return a list of matching addresses" in {
      val controller = new AddressLookupController with DataWS
      val request = FakeRequest(GET,
        "/address-lookup-demo/address-lookup-selection?UK-postcode=AA1AA1"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("AA1AA1")
    }

    "A postcode with >50 addresses will display an error message" in {
      val controller = new AddressLookupController with Data60ItemsWS
      val request = FakeRequest(GET,
        "/address-lookup-demo/address-lookup-selection?UK-postcode=AA1AA1"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("Over 50 addresses found")
    }

    "A postcode with 'random' data will display an error message" in {
      val controller = new AddressLookupController with DummyBadRequestWS
      val request = FakeRequest(GET,
        "/address-lookup-demo/address-lookup-selection?house-name-number=&UK-postcode=nfjewk"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("The postcode was unrecognised")
    }

    "check if 'continue' is selected we return edit fields" in {
      val controller = new AddressLookupController with DummyWS
      val request = FakeRequest(GET,
        "/address-lookup-demo/address-lookup-selection?UK-postcode=AA1AA1"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText mustNot include("Address line 1")
    }

    "display warning if no postcode entered" in {
      val controller = new AddressLookupController with DummyWS
      val request = FakeRequest(GET,
        "/address-lookup-demo/address-lookup-selection"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText mustNot include("A post code is required")
    }

    "check 'no fixed address'" in {
      val controller = new AddressLookupController with DataWS
      val request = FakeRequest(GET,
        "/address-lookup-demo/address-lookup-selection?no-fixed-address=true&UK-postcode=''"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken).withFormUrlEncodedBody("no-fixed-address" -> "true")
      val result = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("Application complete")
      bodyText must include("No fixed address")
    }

  }

  "completion page is displayed" should {
    "we press continue on the list of addrs" in {
      val controller = new AddressLookupController with DummyWS
      val request = FakeRequest(GET,
        "http://localhost:9000/address-lookup-demo/address-lookup-selection?house-name-number=&UK-postcode=AA1AA1&radio-inline-group=GB00001"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result: Future[Result] = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("Application complete")
    }
  }

  "completion page with data filled in form DB is displayed" should {
    "we press continue on the list of addrs" in {
      val controller = new AddressLookupController with DummyListWS
      val request = FakeRequest(GET,
        "http://localhost:9000/address-lookup-demo/address-lookup-selection?house-name-number=&UK-postcode=AA1 AA1&radio-inline-group=GB00001"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result: Future[Result] = controller.addressLookupSelection().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("Application complete")
    }
  }

  "international completion page" should {
    "be displayed when we press continue" in {
      val controller = new AddressLookupController with DummyWS
      val request = FakeRequest(GET,
        "http://localhost:9000/address-lookup-demo/address-lookup-int-selection?int-country=Cuba&int-address=AAA"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result: Future[Result] = controller.intContinueButton().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("Application complete")
      bodyText must include("AAA")
      bodyText must include("Country: Cuba")
    }
  }


  "bfpo addressLookup action" should {
    "find address given a valid bfpo post code" in {
      val controller = new AddressLookupController with DummyWSBFPOWithData
      val request = FakeRequest(GET,
        "http://localhost:9000/address-lookup-demo/address-lookup-bfpo-selection?hiddentab=bfpotab&BFPO-postcode=BF1+3AA"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result: Future[Result] = controller.bfpoContinueButton().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("BFPO post code")
      bodyText must include("BF1 3AA")
      bodyText must include("Op Test")
    }

    "edit address given a valid bfpo post code" in {
      val controller = new AddressLookupController with DummyWSBFPOWithData
      val request = FakeRequest(POST,
        "http://localhost:9000/address-lookup-demo/address-lookup-bfpo-selection?hiddentab=bfpotab&BFPO-postcode=BF1+3AA"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result: Future[Result] = controller.bfpoEditButton().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("BFPO post code")
      bodyText must include("BF1 3AA")
    }

    "edit address given a invalid bfpo post code" in {
      val controller = new AddressLookupController with DummyWSBFPOWithInvalidPostCode
      val request = FakeRequest(POST,
        "http://localhost:9000/address-lookup-demo/address-lookup-bfpo-selection?hiddentab=bfpotab&BFPO-postcode=AA1 1ZZ"
      ).withSession("csrfToken" -> CSRF.SignedTokenProvider.generateToken)
      val result: Future[Result] = controller.bfpoContinueButton().apply(request)

      val bodyText: String = contentAsString(result)
      bodyText must include("BFPO post code")
      bodyText must include("Invalid BFPO postcode found")
      bodyText must include("AA1 1ZZ")
    }


  }
}



trait DummyWS extends AddressLookupWS with DummyBfpoWS {
  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[services.Address]]]] = {
    Future.successful(Right(Some(List[services.Address]())))
  }
}

trait DummyListWS extends AddressLookupWS with DummyBfpoWS {
  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[services.Address]]]] = {
    Future.successful(Right(Some(List[services.Address]( services.Address("GB00001",  Array[String]("line1","line2","line3"), "ATown", "AA1 AA1")))))
  }
}

trait DataWS extends AddressLookupWS with DummyBfpoWS {
  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[services.Address]]]] = {
    Future.successful(Right(Some(List[services.Address](
      services.Address("GB00001", Array[String](""), "ATown", "AA1AA1")
    ))))
  }
}

trait Data60ItemsWS extends AddressLookupWS with DummyBfpoWS {
  val ListSize = 60

  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[services.Address]]]] = {
    Future.successful(Right(Some(List.fill[services.Address](ListSize) {
      services.Address("GB00001", Array[String](""), "ATown", "AA1AA1")
    })))
  }
}

trait DummyBadRequestWS extends AddressLookupWS with DummyBfpoWS {
  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[services.Address]]]] = {
    Future.successful(Left(BadRequest))
  }
}



trait DummyBfpoWS  extends BfpoLookupWS {
  def findBfpo(postcode: String): Future[Either[Status, Option[List[BfpoDB]]]] = ???
}


trait DummyWSBFPOWithData extends AddressLookupWS with DummyBfpoWS {
  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[services.Address]]]] = ???

  override def findBfpo(postcode: String): Future[Either[Status, Option[List[BfpoDB]]]] = {
    Future.successful(Right(Some(List[services.BfpoDB](BfpoDB(Some("Op Test"), "123", "BF1 3AA")))))
  }
}

trait DummyWSBFPOWithInvalidPostCode extends AddressLookupWS with DummyBfpoWS {
  def findAddresses(postcode: String, filter: Option[String]): Future[Either[Status, Option[List[services.Address]]]] = ???

  override def findBfpo(postcode: String): Future[Either[Status, Option[List[BfpoDB]]]] = {
    Future.successful(Left(BadRequest))
  }
}
