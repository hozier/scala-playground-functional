object Functional {
  object Operations {
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
    def main(args: String*): Unit = {
      Seq(
        s"\n[Functional Scala]",
        s"doSum=${Operations.doSum((0 to 31).toList)}",
        s"countFizz=${Operations.countFizz((0 to 31).toList)}",
        s"tailRecFactorial=${Operations.tailRecFactorial(3, 1)}",
        s"foldToSet=${Operations.foldToSet(List(2, 3, 2, 3))}",
        s"balanceParens=${Exercises.Operations.balance("(if (zero? x) max (/ 1 x))")}",
        s"balanceParens=${Exercises.Operations.balance("I told him (that it’s not (yet) done). (But he wasn’t listening)")}",
        s"balanceParens=${Exercises.Operations.balance(":-)")}",
        s"balanceParens=${Exercises.Operations.balance("())(")}",
        s"balanceParens=${Exercises.Operations.balance("(()))")}",
        s"balanceParens=${Exercises.Operations.balance("((())")}"
      )
        .foreach(println)
    }
  }
}
