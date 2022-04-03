package concepts

object Fundamentals {
  object Session {
    def doSum(in: List[Int]): Int = in.fold(0)((acc, item) => acc + item)
    def countFizz(universe: List[Int], FIZZ: Int = 3): Int = {
      universe.filter(x => x % FIZZ == 0).size
    }

    /** @note for inputs 1 and greater. */
    def tailRecFactorial(i: Int, acc: Int): Int = {
      if (i < 2) acc
      else tailRecFactorial(i - 1, i * acc)
    }
    def foldToSet(xs: List[Int]): Set[Int] = {
      xs.foldLeft(Set.empty[Int])((acc: Set[Int], item: Int) => acc + item)
    }
    def main: () => Unit = () => {
      Seq(
        s"\n[Scala FP Fundamentals]",
        s"doSum=${doSum((0 to 31).toList)}",
        s"countFizz=${countFizz((0 to 31).toList)}",
        s"tailRecFactorial=${tailRecFactorial(3, 1)}",
        s"foldToSet=${foldToSet(List(2, 3, 2, 3))}"
      )
        .foreach(println)
    }
  }
  object Exercise {
    def isBalancedParens: (Seq[Char], List[Char]) => Int = (chars, record) => {
      if (chars.size == 0) record.size
      else {
        isBalancedParens(
          chars.drop(1),
          if (chars(0).equals(')') && !record.isEmpty) record.drop(1)
          else chars(0) :: record
        )
      }
    }
    def balance: String => Boolean = str => {
      val parens = str.toCharArray
        .filter(char => char.equals('(') || char.equals(')'))
        .toSeq

      isBalancedParens(parens, List()) match {
        case 0 => true
        case _ => false
      }
    }
  }
}
