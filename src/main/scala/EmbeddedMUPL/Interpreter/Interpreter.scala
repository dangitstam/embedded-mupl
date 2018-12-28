
package EmbeddedMUPL.Interpreter

import EmbeddedMUPL.Language.MUPL._

// TODO: Use List[String, Exp] instead of map

object Interpreter {

    def envLookUp(variable: String, env: List[(String, Exp)]): Option[Exp] = {
        env.find( _._1 == variable ) match {
            case Some((s, v)) => Some(v)
            case _          => None
        }
    }

    /**
     * Interprets the AST into an Option[Int]. None is returned
     * on faulty math operations.
     */
    @throws(classOf[ArithmeticException])
    def evalUnderEnv(ast: Exp, env: List[(String, Exp)]): Exp = ast match {
        case Var(s) => envLookUp(s, env) match {
            case Some(e)   => evalUnderEnv(e, env)
            case None      => throw new BadMUPLExpression("Undefined variable %s".format(s))
        }
        case Munit() => ast
        case Const(i) => ast
        case Add(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
            case (Const(i), Const(j)) => Const(i + j)
            case _  => throw new BadMUPLExpression("Addition includes non-Const")
        }
        case Subtract(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
            case (Const(i), Const(j)) => Const(i - j)
            case _  => throw new BadMUPLExpression("Subtraction includes non-Const")
        }
        case Multiply(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
            case (Const(i), Const(j)) => Const(i * j)
            case _  => throw new BadMUPLExpression("Multiplication includes non-Const")
        }
        case Divide(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
            case (Const(i), Const(j)) =>
                if (j > 0) {
                    Const(i / j)
                } else {
                    throw new BadMUPLExpression("Division by zero")
                }
            case _  => throw new BadMUPLExpression("Multiplication includes non-Const")
        }

        // Pair & list semantics.
        case Apair(e1, e2) => Apair(evalUnderEnv(e1, env),
                                    evalUnderEnv(e2, env))
        case First(e) => evalUnderEnv(e, env) match {
            case Apair(e1, e2) => e1
            case _ => throw new BadMUPLExpression("First applied to non-pair")
        }
        case Second(e) => evalUnderEnv(e, env) match {
            case Apair(e1, e2) => e2
            case _ => throw new BadMUPLExpression("Second applied to non-pair")
        }

        // Comparisons and control flow.
        case IsMunit(e) => evalUnderEnv(e, env) match {
            case Munit() => Const(1)
            case _       => Const(0)
        }
        case IsGreater(a, b) => (evalUnderEnv(a, env), evalUnderEnv(b, env)) match {
            case (Const(i), Const(j)) =>
                if (i > j) {
                    Const(1)
                } else {
                    Const(0)
                }
            case _  => throw new BadMUPLExpression("Greater-than applied to non-Const")
        }

        case Ifnz(cond, e1, e2) => evalUnderEnv(cond, env) match {
            case Const(i) =>
                if (i != 0) {
                    evalUnderEnv(e1, env)
                } else {
                    evalUnderEnv(e2, env)
                }
            case _  => throw new BadMUPLExpression("If-not-zero applied to non-Const")
        }

        // Variable bindings.
        case Let(name: Var, value: Exp, body: Exp) => {
            val Var(s) = name;
            evalUnderEnv(body, (s, evalUnderEnv(value, env)) :: env)
        }
        
        // Function call semantics.
        case Fun(fname, argname: Var, body: Exp) => {
            Closure(env, Fun(fname, argname, body))
        }
        case Closure(env, fn) => Closure(env, fn)
        case Call(fn: Exp, arg: Exp) => evalUnderEnv(fn, env) match {
            case Closure(fnEnv, fn) => {
                val Fun(fnNameVariable, Var(fnArgName), fnBody) = fn

                // Create a mapping for the evaluated function argument.
                var bodyEnv = (fnArgName, evalUnderEnv(arg, env)) :: fnEnv

                // Mapping named functionf in the environment allows recursion.
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

    def eval(ast: Exp): Exp = evalUnderEnv(ast, List())

    /**
     * Custom exceptions.
     */
    final case class BadExp(
        private val message: String = "", 
        private val cause: Throwable = None.orNull)
        extends Exception(message, cause)
    
    final case class DivisionByZero(
        private val message: String = "Division by zero", 
        private val cause: Throwable = None.orNull)
        extends Exception(message, cause)

    final case class BadMUPLExpression(
        private val message: String = "", 
        private val cause: Throwable = None.orNull)
        extends Exception(message, cause)

    /**
     * Trick from Phillp Gabler
     *
     * Iterates over a pair of Option values and applies a function to their
     * values if they resolve to Some[A] and Some[B].
     */
    def join[A, B](a: Option[A], b: Option[B]) = new {
        // For operates over any collection, and options are a
        // collection containing either 0 or 1 things.
        def by[C](f: (A, B) => C): Option[C] = for {
            x <- a
            y <- b
        } yield f(x, y)
    }
}