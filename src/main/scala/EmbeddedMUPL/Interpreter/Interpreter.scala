package EmbeddedMUPL.Interpreter

import EmbeddedMUPL.Language.MUPL._


object Interpreter {

  /**
   * Interprets the AST to a value (Const / Munit) beginning from an
   * empty environment.
   */
  def eval(ast: Exp): Exp = evalUnderEnv(ast, List())

  private def envLookUp(variable: String, env: List[(String, Exp)]): Option[Exp] = {
    /* In Scala, _ is the wild card operator.
     * The condition _._1 == variable when given to `find` will match against
     * every element in `env`, calls the method _1 (which extracts the first
     * element of the matched expression), and returns a boolean as to whether
     * that first element equals `variable`. This is conventional Scala for
     * doing lookings when your map is a list of pairs.
     *
     * Since our environment consists of mappings from string to expr, for a
     * given string s, this method returns Some(v) if s->v exists and
     * None otherwise. */
    env.find( _._1 == variable ) match {
      case Some((s, v)) => Some(v)
      case _ => None
    }
  }

  @throws(classOf[ArithmeticException])
  private def evalUnderEnv(ast: Exp, env: List[(String, Exp)]): Exp = ast match {
    /* This method interprets the AST to a value under the current environment.
     *
     * Although it is less efficient to use a `List[(String, Exp)]` as our
     * environment (linear search every time we do a lookup) we allow this to
     * gain a significantly simpler way to maintain proper shadowing.
     * More specifically, elements that appear sooner in the list represnetation
     * will shadow elements later in the list. We will depend on this when
     * implementing lexical scope.
     *
     * The resulting expression will either be a Const(i) or an Munit()
     * (the MUPL analog for null/empty). In any other instance, an exception is
     * thrown.
     *
     * Custom exceptions are used to help further distinguish between embedded
     * MUPL code and conventional Scala code. */
    case Var(s) => envLookUp(s, env) match {
      case Some(e) => evalUnderEnv(e, env)
      case None => throw new BadMUPLExpression("Undefined variable %s".format(s))
    }
    case Munit() => ast
    case Const(i) => ast

    /* Arithmetic operations are intuitively how you'd expect them to be.
      *
      * Note that
      * 1. It only makes sense to perform arithmetic with two `Const` values.
      * 2. It could be the case that an expression, when recursively evaluated,
      *    results in a `Const`. Such expressions should be evaluated before the
      *    actual addition.
      * 3. Division by zero shouldn't be allowed.
      *
      * The following operations are implemented to reflect this, recursively
      * evaluating subexpressions and only doing the arithmetic when
      * appropriate. */
    case Add(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
      case (Const(i), Const(j)) => Const(i + j)
      case _ => throw new BadMUPLExpression("plus includes non-Const")
    }
    case Subtract(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
      case (Const(i), Const(j)) => Const(i - j)
      case _ => throw new BadMUPLExpression("minus includes non-Const")
    }
    case Multiply(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
      case (Const(i), Const(j)) => Const(i * j)
      case _ => throw new BadMUPLExpression("times includes non-Const")
    }
    case Divide(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
      case (Const(i), Const(j)) =>
        if (j > 0) {
          Const(i / j)
        } else {
          throw new DivisionByZero()
        }
      case _ => throw new BadMUPLExpression("times includes non-Const")
    }

    /* Pairs and lists are implemented as Racket-style Church lists, where a
     * list is represented as a pair containing one element and another list.
     * 
     * `munit` represents both the empty list, and the end of a populated list.
     * More concretely, the AST `Pair(Const(1), Munit())` represents a single
     * element list containing `Const(1)`, whereas `Munit()` represents an
     * empty list. Empty lists are never represented with Pair.
     *
     * `First`, when given a pair, extracts the first element of the list.
     * `Second` extracts the "tail" of the list (second element of the pair).
     * In either case, it only makes sense to evaluate the element that you
     * need! */
    case Apair(e1, e2) => Apair(evalUnderEnv(e1, env), evalUnderEnv(e2, env))
    case First(e) => evalUnderEnv(e, env) match {
      case Apair(e1, e2) => e1
      case _ => throw new BadMUPLExpression("first applied to non-pair")
    }
    case Second(e) => evalUnderEnv(e, env) match {
      case Apair(e1, e2) => e2
      case _ => throw new BadMUPLExpression("second applied to non-pair")
    }

    /* Comparisons and control flow.
     * It's best to refer to the MUPL API with regards to what these operations
     * do: https://courses.cs.washington.edu/courses/cse341/18au/hw5.pdf
     *
     * To quickly describe what each of these do, note that embedded MUPL
     * has no explicit boolean type. We use `Const(1)` to mean true and
     * `Const(0)` to mean false.
     *
     * IsMunit - returns true of the evaluated expression is an Munit.
     * IsGreater - returns true if its elements are both `Const` and that
     * the first element is larger than the second.
     * Ifnz - serves as MUPL's if-then-else control flow, such that a true
     * condition (hence the name ifnz = if not zero) will yield the true
     * clause, e1. If the condition is zero, it yields the false clause, e2. */
    case IsMunit(e) => evalUnderEnv(e, env) match {
      case Munit() => Const(1)
      case _ => Const(0)
    }
    case IsGreater(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
      case (Const(i), Const(j)) =>
        if (i > j) {
          Const(1)
        } else {
          Const(0)
        }
      case _ => throw new BadMUPLExpression("isgreater applied to non-Const")
    }
    case Ifnz(cond, e1, e2) => evalUnderEnv(cond, env) match {
      case Const(i) =>
        if (i != 0) {
          evalUnderEnv(e1, env)
        } else {
          evalUnderEnv(e2, env)
        }
      case _  => throw new BadMUPLExpression("ifnz applied to non-Const")
    }

    /* Let expressions create a new variable binding in which later
     * expressions refer to. With a let expression, a new binding in the
     * environment is created.
     * 
     * For instance, in the following embedded MUPL program:
     *  val program = (let variable "x" equal 12 in ( "x" minus 17 )) plus 
     *                (let variable "y" equal 12 in ( "y" times 2 ))
     *
     * Let bodies `"x" minus 17` and `"y" times 2` are free to use "x" and "y"
     * as variables. Lexical scope applies: unless either "x" or "y" were
     * defiend before these expressions, "x" and "y" will only be defined
     * within the bodies of the let expressions with which they were
     * defined and no where else.
     *
     * Of course, this also lends itself to nesting let expressions so that
     * more variables can be in scope:
     *   val program =
     *   (let variable "x" equal 12 in (
     *       (let variable "y" equal 12 in (
     *           "x" times "y"
     *           )
     *       )
     *   )
     */
    case Let(name: Var, value: Exp, body: Exp) => {
      val Var(s) = name;  // Match against the name for extracting the string.
      evalUnderEnv(body, (s, evalUnderEnv(value, env)) :: env)
    }
    
    /* Function call semantics.
     * Similar to control flow, it's best to refer to the original MUPL API
     * to understand the underpinnings behind the implementation of function
     * calls here.
     *
     * In short, the following implementation evaluates function arguments and
     * maps them to the funtion's argument name in the environment. If a
     * function is not anonymous (i.e. it was defined with an explicit name)
     * then a mapping in the environment from the function name to the function
     * itself is also created.
     *
     * Closures allow the encapsulation of the environment in which a function
     * is defined. Function bodies in MUPL are always evaluated in the
     * environment in which they were defined.
     */
    case Fun(fname, argname: Var, body: Exp) => {
      Closure(env, Fun(fname, argname, body))
    }
    case Closure(env, fn) => Closure(env, fn)
    case Call(fn: Exp, arg: Exp) => evalUnderEnv(fn, env) match {
      case Closure(fnEnv, fn) => {
        val Fun(fnNameVariable, Var(fnArgName), fnBody) = fn

        // Create a mapping for the evaluated function argument.
        var bodyEnv = (fnArgName, evalUnderEnv(arg, env)) :: fnEnv

        // Mapping named functions in the environment allows recursion.
        if (fnNameVariable != null) {
          val Var(fnName) = fnNameVariable
          bodyEnv = (fnName, Closure(fnEnv, fn)) :: bodyEnv
        }

        evalUnderEnv(fnBody, bodyEnv)
      }
      case _ => {
        throw new BadMUPLExpression("Function call applied with non-function")
      }
    }
  }

  /**
   * Custom exceptions.
   */
  final case class DivisionByZero(
    private val message: String = "Division by zero", 
    private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  final case class BadMUPLExpression(
    private val message: String = "", 
    private val cause: Throwable = None.orNull)
    extends Exception(message, cause)
}
