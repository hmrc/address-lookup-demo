package services

/**
  * Created by andy on 08/04/2016.
  */
object MyHelpers {
  import views.html.helper.FieldConstructor
  implicit val myFields = FieldConstructor(views.html.addresslookup.defaultAddressLookupFieldConstructor.f)
}