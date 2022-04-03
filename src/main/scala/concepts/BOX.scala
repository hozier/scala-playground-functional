package concepts

object IO {
  object Exercise {

    /** @note
      *   implementation of "generic" monad type, mocks functionality of
      *   cats-effect IO monad
      */
    trait BOX[A] { self =>
      def run(): A

      /** task: implement map, scala docs signature:
        *
        * [B](f: A => B): BOX[B]
        */
      def map[B]: (A => B) => BOX[B] = f =>
        BOX {
          f(self.run())
        }

      /** task: implement map, scala docs signature:
        *
        * [B](f: A => BOX[B]): BOX[B]
        */
      def flatMap[B]: (A => BOX[B]) => BOX[B] = f => f(self.run())
    }

    object BOX {

      /** task: lazily implement apply via call by name */
      def apply[A](delayed: => A): BOX[A] = new BOX[A] {
        def run(): A = delayed
      }
    }

    object BOXTest extends App {

      /** Show that map and flatmap works
        *
        * Show that BOX wont run anything until explicitly calling run
        */
      def main: () => Unit = () => {
        (for {
          hello <- BOX(delayed = "Hello")
          helloWorld <- BOX(hello + " world")
          printed <- BOX(println(helloWorld))
        } yield printed).run()
      }
    }
  }
}
