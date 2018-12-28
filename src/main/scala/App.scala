
import EmbeddedMUPL.Interpreter._
import EmbeddedMUPL.Language.MUPL._
import EmbeddedMUPL.Language.MUPL.Enhancements._

object App {
  def main(args: Array[String]) {
    import Interpreter._

    // Embedded MUPL!
    val x =
    let variable "x" equal 12 in (
        let variable "y" equal 14 in (
            "x" plus "y"
        )
    )

    var f =
    let function null of "x" be (
        "x" times 17
    )

    val program = apply function f on x

    println(program)
    println(eval(program))
  }
}
