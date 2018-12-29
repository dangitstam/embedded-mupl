
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
    define function null of "x" as (
        ifnz (isgreater ("x", 12 + 25))
        then ("x" times 17)
        otherwise Const(1000)
    )

    val program = apply function f on x

    println(program)
    println(eval(program))
  }
}
