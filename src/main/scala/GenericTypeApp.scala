object GenericTypeApp extends App {

  import shapeless.{HList, ::, HNil}

  val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil
}
