object Fundamentals {
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
        s"\n[Scala FP Fundamentals]",
        s"doSum=${doSum((0 to 31).toList)}",
        s"countFizz=${countFizz((0 to 31).toList)}",
        s"tailRecFactorial=${tailRecFactorial(3, 1)}",
        s"foldToSet=${foldToSet(List(2, 3, 2, 3))}"
      )
        .foreach(println)
    }
  }
}
