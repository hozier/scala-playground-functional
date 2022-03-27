object Exercises {
  object Operations {

    /** Write a recursive function which verifies the balancing of parentheses
      * in a string. True for:
      *   - (if (zero? x) max (/ 1 x))
      *   - I told him (that it’s not (yet) done). (But he wasn’t listening)
      *     False for:
      *   - :-)
      *   - ())( You can use:
      *   - chars.isEmpty
      *   - chars.head
      *   - chars.tail
      * @param chars
      *   List[Char] not a String
      * @return
      *   true/false if parentheses are balanced
      */
    def isBalancedParens(chars: Seq[Char], record: List[Char]): Int = {
      if (chars.size == 0) record.size
      else {
        isBalancedParens(
          chars.drop(1),
          if (chars(0).equals(')') && !record.isEmpty) record.drop(1)
          else chars(0) :: record
        )
      }
    }
    def balance(str: String): Boolean = {
      val parens = str.toCharArray
        .filter(char => char.equals('(') || char.equals(')'))

      isBalancedParens(parens, List()) match {
        case 0 => true
        case _ => false
      }
    }
    def main(): Unit = {
      Seq(
        s"\n[Scala FP Fundamentals (Excercises)]",
        s"balanceParens=${balance("(if (zero? x) max (/ 1 x))")}",
        s"balanceParens=${balance("I told him (that it’s not (yet) done). (But he wasn’t listening)")}",
        s"balanceParens=${balance(":-)")}",
        s"balanceParens=${balance("())(")}",
        s"balanceParens=${balance("(()))")}",
        s"balanceParens=${balance("((())")}"
      )
        .foreach(println)
    }
  }
}
