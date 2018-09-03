import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers, Succeeded}

class PropertyTestSpec extends FlatSpec
  with Matchers with GeneratorDrivenPropertyChecks
  /*Use PropertyChecks instead of GeneratorDrivenPropertyChecks*/ {
  implicit val arbString: Arbitrary[String] = Arbitrary(Gen.alphaStr.suchThat(_.nonEmpty))
  it should "generate random data" in new GeneratorDrivenPropertyChecks {
    forAll { (s: String) =>
      println(s)
      s shouldBe s
    }
  }

  it should "generate list of random data" in {
    forAll { (s: List[String]) =>
      s shouldBe s
    }
  }

  it should "without GeneratorDrivenPropertyChecks " in {
    implicit def assertion(as: Assertion): Prop = Prop(as == Succeeded)

    Prop.forAll { (s: List[String]) =>
      println(s)
      s shouldBe s
    }
  }

  it should "check case class random data for non-negative" in {
    case class Person(name: String, age: Int)
    def doSomething(p: Person) = true

    val people = Gen.resultOf(Person)
    forAll(people) { p =>
      whenever(p.age >= 0) {
        doSomething(p)
      }
    }
  }

  it should "Generate data for case class using GeneratorDrivenPropertyChecks" in {
    case class Test(a: String, b: Int)
    implicit val test: org.scalacheck.Arbitrary[Test] = Arbitrary(Gen.resultOf(Test))
    forAll { (t: Test) =>
      t.a shouldBe t.a
    }
  }

}
