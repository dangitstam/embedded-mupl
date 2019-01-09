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

  /**
   * Control flow.
   *
   * Custom if-not-zero:
   * e1 ifnz cond otherwise e2
   */
  def ifnz(cond: Exp) = new Otherwise(cond, this)
}

/**
 * Original MUPL constructs.
 * Composition of these constructs make up the ASTs that
 * are built from the DSL syntax.
 *
 * ASTs made from these constructs are either interpreted
 * or compiled to Python.
 */
final case class Var(s: String)              extends Exp
final case class Const(i: Int)               extends Exp
final case class Add(e1: Exp, e2: Exp)           extends Exp
final case class IsGreater(e1: Exp, e2: Exp)         extends Exp
final case class Ifnz(e1: Exp, e2: Exp, e3: Exp)       extends Exp
final case class Call(e1: Exp, e2: Exp)          extends Exp
final case class Fun(s1: Var, s2: Var, e: Exp)       extends Exp
final case class Let(name: Var, value: Exp, body: Exp)   extends Exp
final case class Apair(e1: Exp, e2: Exp)           extends Exp
final case class First(e: Exp)               extends Exp
final case class Second(e: Exp)              extends Exp
final case class Munit()                   extends Exp
final case class IsMunit(e: Exp)               extends Exp
final case class Closure(env: List[(String, Exp)], f: Fun) extends Exp

/**
 * Additions to the MUPL language.
 */
final case class Subtract(a: Exp, b: Exp) extends Exp
final case class Multiply(a: Exp, b: Exp) extends Exp
final case class Divide(a: Exp, b: Exp) extends Exp

// For use in ifnz only:
class Otherwise(cond: Exp, e1: Exp) {
  def otherwise(e2: Exp) = new Ifnz(cond, e1, e2)
}

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
   * Comparisons.
   */
  def isgreater(e1: Exp, e2: Exp) = IsGreater(e1, e2)

  /**
   * Method calls using `.` can also be called with a space
   * (i.e. foo.x and foo x).
   * We can chain function calls and object instantiation to create new syntax
   * embedded within Scala.
   *
   * For instance, below we have custom let syntax:
   *    let variable "x" equal ("x" plus "y") in ("z" minus 14 divide "z")
   * And function syntax
   *    define function null of "x" as (
   *       "x" times 17
   *    )
   */ 
  def let = new LetConstructor()
  class LetConstructor() {
    def variable(name: Var): Eq = new Eq(name)
  }
  def define = new FunctionConstructor()
  class FunctionConstructor() {
    def function(name: Var): Eq = new Eq(name)
  }
  class Eq(name: Var) {
    def equal(value: Exp) = new In(name, value)
    def of(arg: Var) = new As(name, arg)
  }
  class In(name: Var, value: Exp) {
    def in(body: Exp): Exp = Let(name, value, body)
  }
  class As(name: Var, arg: Var) {
    def as(body: Exp): Exp = Fun(name, arg, body)
  }

  /**
   * Custom function call:
   * apply function "f" on ("x" plus 12)
   */
  def apply = new CallConstructor()
  class CallConstructor() { def function(fname: Exp): Use = new Use(fname) }
  class Use(fname: Exp) { def on(arg: Exp) = new Call(fname, arg) }

  /**
   * Alternative if-not-zero:
   * ifnz(cond) then e1 else e2 
   */
   def ifnz(cond: Exp) = new IfnzConstructor(cond)
   class IfnzConstructor(cond: Exp) {
     def then(e1: Exp) = new Otherwise(cond, e1)
   }

   /**
    * List construction and semantics.
    * apair e1 e2
    */
    def construct = new CollectionsConstructor()
    class CollectionsConstructor() {
      def pair(e1: Exp, e2: Exp) = Apair(e1, e2)
    }
    def first(e: Exp) = First(e)
    def second(e: Exp) = Second(e)
    def munit() = Munit()
    def ismunit(e: Exp) = IsMunit(e)
}
