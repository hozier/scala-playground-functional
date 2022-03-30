package concepts

object IO {
  object Exercise {

    /** @note
      *   implementation of "generic" monad type, mocks functionality of
      *   cat-effect's IO monad
      */
    trait MONAD[A] { self =>
      def run(): A

      /** task: implement map, scala docs signature:
        *
        * [B](f: A => B): ${MONAD}[B]
        */
      def map[B]: (A => B) => MONAD[B] = f =>
        MONAD {
          f(self.run)
        }

      /** task: implement map, scala docs signature:
        *
        * [B](f: A => ${MONAD}[B]): ${MONAD}[B]
        */
      def flatMap[B]: (A => MONAD[B]) => MONAD[B] = f => f(self.run)
    }

    object MONAD {

      /** task: lazily implement apply via call by name */
      def apply[A]: (delayed => A) => MONAD[A] = delayed = {
        new MONAD[A] {
          def run(): A = delayed
        }
      }
    }

    object MONADTest extends App {

      // Show that map and flatmap works

      // Show that MONAD wont run anything until explicitly calling run
    }
  }
}
