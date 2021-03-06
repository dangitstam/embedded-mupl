
import EmbeddedMUPL.Interpreter._
import EmbeddedMUPL.Compiler._
import EmbeddedMUPL.Language.MUPL._
import EmbeddedMUPL.Language.MUPL.Enhancements._


object App {
  def main(args: Array[String]) {
    import Interpreter._
    import Compiler._

    // Embedded MUPL! Below are several examples in how the DSL
    // can be used.

    val x =
    let variable "x" equal 12 in (
        let variable "y" equal 14 in (
            "x" plus "y"
        )
    )

    val x2 =
    (let variable "x" equal 12 in (
        "x" minus 17
    )) plus (let variable "y" equal 12 in (
        "y" times 2
    ))

    var f =
    define function null of "x" as (
        ifnz (isgreater ("x", 12 + 25))
        then ("x" times 17)
        otherwise Const(1000)
    )

    var f2 =
    define function "z" of "x" as (
        "x" plus 23
    )

    var f3 =
    (ifnz (Const(12))
     then (29 plus 17)
     otherwise Const(1000))

    val program = apply function f on x

    val pair = construct pair(f3, munit)

    // Interpreted (produces an immediate result).
    val Const(value) = eval(program)
    println("res = %d".format(value))

    // Compiled to Python.
    // When run in a python interperter, also produces res = 1000.
    //
    // Adding `print("res = {}".format(res))` at the end of the resulting
    // `mupl.py` in your working directory will show this!
    compile(program)
  }
}
