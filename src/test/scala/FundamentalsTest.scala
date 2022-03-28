import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class Spec extends AnyWordSpec with Matchers {
  "FPPart02" should {
    "return true for balanced paren cases" in {
      Exercises.Operations.balance("()") should equal(true)
    }

    "return true for a complex but correct lisp expression" in {
      val complexLispExpression = """
      (defun fibonacci (N)
        "Compute the N'th Fibonacci number."
        (if (or (zerop N) (= N 1))
            1
          (+ (fibonacci (- N 1)) (fibonacci (- N 2)))))
      """

      Exercises.Operations.balance(complexLispExpression) should equal(
        true
      )
    }

    "should return false for an unbalanced expression" in {
      Exercises.Operations.balance(
        "(wat (oh (i forgot a close paren))"
      ) should equal(false)
    }

    "should return false for an expression with a close before open" in {
      Exercises.Operations.balance(")(") should equal(false)
    }

    "should return false for a nested unbalanced expression" in {
      Exercises.Operations.balance(
        "(wat (oh (i forgot a close paren))"
      ) should equal(false)
    }

    "should return true for a nested balanced expression" in {
      Exercises.Operations.balance(
        """I told him (that it’s not (yet) done). (But
      * he wasn’t listening)"""
      ) should equal(true)
    }

    "should return true for a balanced expression" in {
      Exercises.Operations.balance("(if (zero? x) max (/ 1 x))") should equal(
        true
      )
    }

    "should return false for a simple unbalanced expression" in {
      Exercises.Operations.balance(":-)") should equal(false)
    }

    "should return false for a simple unbalanced containing only parens expression" in {
      Exercises.Operations.balance("())(") should equal(false)
    }
  }
}
