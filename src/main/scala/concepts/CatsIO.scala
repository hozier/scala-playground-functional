package concepts
import cats.effect.IO

object CatsIO {
  object Operations {
    def say(): IO[String] = IO.delay("Hello Cats!")
    def main: () => Unit = () => {
      import cats.effect.IOApp
      import cats.effect.IO
      object Main extends IOApp.Simple {

        // This is your new "main"!
        def run: IO[Unit] =
          say().flatMap(IO.println)
      }
    }
  }
}
