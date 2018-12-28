package EmbeddedMUPL.Language.MUPL

import scala.language.reflectiveCalls
import scala.language.implicitConversions
import java.lang.ArithmeticException


sealed trait Exp {

    /**
     * Defines basic portions of the DSL syntax.
     * Equates infix operators to their AST equivalents.
     */
    def plus(other: Exp) = Add(this, other)
    def minus(other: Exp) = Subtract(this, other)
    def times(other: Exp) = Multiply(this, other)
    def divide(other: Exp) = Divide(this, other)
}

/**
 * Original MUPL constructs.
 */
final case class Var(s: String)                            extends Exp
final case class Const(i: Int)                             extends Exp
final case class Add(e1: Exp, e2: Exp)                     extends Exp
final case class IsGreater(e1: Exp, e2: Exp)               extends Exp
final case class Ifnz(e1: Exp, e2: Exp, e3: Exp)           extends Exp
final case class Call(e1: Exp, e2: Exp)                    extends Exp
final case class Fun(s1: Var, s2: Var, e: Exp)             extends Exp
final case class Let(name: Var, value: Exp, body: Exp)     extends Exp
final case class Apair(e1: Exp, e2: Exp)                   extends Exp
final case class First(e: Exp)                             extends Exp
final case class Second(e: Exp)                            extends Exp
final case class Munit()                                   extends Exp
final case class IsMunit(e: Exp)                           extends Exp
final case class Closure(env: List[(String, Exp)], f: Fun) extends Exp

/**
 * Additions to the MUPL language.
 */
final case class Subtract(a: Exp, b: Exp) extends Exp
final case class Multiply(a: Exp, b: Exp) extends Exp
final case class Divide(a: Exp, b: Exp) extends Exp

/**
 * Extend the language further to include implicit variable,
 * constant, and function creation.
 */
 object Enhancements {

     /** 
      * The presence of these implicit definitions allow
      * using strings and ints in place of Var(string)
      * and Const(int).
      *
      * For instance, "x" plus "y" is implicitly converted to
      * Var("x") plus Var("y") when plus is called.
      */
    implicit def StringToExp(s: String): Var = Var(s)
    implicit def intToExp(i: Int): Const = Const(i)

    /**
     * Custom let expresion:
     * let variable "x" equal ("x" plus "y") in ("z" minus 14 divide "z")
     *
     * Custom function definition:
     * let function null of "x" be (
     *     "x" times 17
     * )
     */ 
    def let = new LetConstructor()
    class LetConstructor() {
        def variable(name: Var): Eq = new Eq(name)
        def function(name: Var): Eq = new Eq(name)
    }
    class Eq(name: Var) {
        def equal(value: Exp) = new In(name, value)
        def of(arg: Var) = new Be(name, arg)
    }
    class In(name: Var, value: Exp) { def in(body: Exp): Exp = Let(name, value, body) }
    class Be(name: Var, arg:   Var) { def be(body: Exp): Exp = Fun(name, arg, body) }

    /**
     * Custom function call:
     * apply function "f" on ("x" plus 12)
     */
    def apply = new FunctionConstructor()
    class FunctionConstructor() { def function(fname: Exp): Use = new Use(fname) }
    class Use(fname: Exp)       { def on(arg: Exp) = new Call(fname, arg) }

    /**
     * Custom if-not-zero:
     * ifnz(cond) then e1 otherwise e2
     */
    def ifnz(cond: Exp) = new IfnzConstructor(cond)
    class IfnzConstructor(cond: Exp) {
        def then(e1: Exp): Else = new Else(cond, e1)
    }
    class Else(cond: Exp, e1: Exp) {
        def otherwise(e2: Exp) = new Ifnz(cond, e1, e2)
    }
 }
