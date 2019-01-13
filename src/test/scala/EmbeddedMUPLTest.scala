import EmbeddedMUPL.Interpreter._
import EmbeddedMUPL.Compiler._
import EmbeddedMUPL.Language.MUPL._
import EmbeddedMUPL.Language.MUPL.Enhancements._

class EmbeddedMUPLTest extends org.scalatest.FunSuite {
  import Interpreter._

  test("EmbededMUPL Addition") {

    val simpleAddition = 1 plus 1
    assert(eval(simpleAddition) == Const(2))

    val commutativity1 = 1 plus 2
    assert(eval(commutativity1) == Const(3))

    val commutativity2 = 2 plus 1
    assert(eval(commutativity2) == Const(3))

    val additionWithVariables =
    let variable "x" equal 1 in (
        "x" plus 2
    )
    assert(eval(additionWithVariables) == Const(3))

    val nestedAdditionWithVariables =
    let variable "x" equal 1 in (
      let variable "y" equal 2 in (
        "x" plus "y"
      )
    )
    assert(eval(nestedAdditionWithVariables) == Const(3))
  }

  test("EmbededMUPL Subtraction") {

    val simpleAddition = 1 minus 1
    assert(eval(simpleAddition) == Const(0))

    val commutativity1 = 1 minus 2
    assert(eval(commutativity1) == Const(-1))

    val commutativity2 = 2 minus 1
    assert(eval(commutativity2) == Const(1))

    val additionWithVariables =
    let variable "x" equal 1 in (
        "x" minus 2
    )
    assert(eval(additionWithVariables) == Const(-1))

    val nestedAdditionWithVariables =
    let variable "x" equal 1 in (
      let variable "y" equal 2 in (
        "x" minus "y"
      )
    )
    assert(eval(nestedAdditionWithVariables) == Const(-1))
  }

  test("EmbededMUPL Ifnz") {

    val condtrue = 
    (ifnz (Const(1))
     then (Const(100))
     otherwise Const(-100))
    assert(eval(condtrue) == Const(100))

    val condfalse = 
    (ifnz (Const(0))
     then (Const(8000))
     otherwise Const(-8000))
    assert(eval(condfalse) == Const(-8000))

    val ifnzEvaluatesCondTrue = 
    (ifnz (let variable "x" equal 1 in (
             let variable "y" equal 2 in (
               "x" plus "y")))
     then (Const(1))
     otherwise Const(0))
    assert(eval(ifnzEvaluatesCondTrue) == Const(1))

    val ifnzEvaluatesCondFalse = 
    (ifnz (let variable "x" equal 1 in (
             let variable "y" equal 1 in (
               "x" minus "y")))
     then (Const(1))
     otherwise Const(0))
    assert(eval(ifnzEvaluatesCondFalse) == Const(0))

    val ifnzEvaluatesTrueClause = 
    (ifnz (Const(1))
     then (let variable "x" equal 1 in (
             let variable "y" equal 1 in (
               "x" plus "y")))
     otherwise Const(0))
    assert(eval(ifnzEvaluatesTrueClause) == Const(2))

    val ifnzEvaluatesFalseClause = 
    (ifnz (Const(0))
     then (Const(0))
     otherwise (let variable "x" equal 1 in (
             let variable "y" equal 1 in (
               "x" plus "y"))))
    assert(eval(ifnzEvaluatesFalseClause) == Const(2))
  }
}
