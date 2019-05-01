package example

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class HelloSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "The Hello object" should "say hello" in {
    forAll { s: String =>
      Hello.greeting shouldEqual "hello"
    }
  }
}
