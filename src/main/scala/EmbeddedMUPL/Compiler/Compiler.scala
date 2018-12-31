package EmbeddedMUPL.Compiler

import EmbeddedMUPL.Language.MUPL._


object Compiler {

    var indentation = 0

    /**
     * Interprets the AST to a value (Const / Munit) beginning from an
     * empty environment.
     */
    def compile(ast: Exp): Unit = compileUnderEnv(ast, List())

    private def envLookUp(variable: String, env: List[(String, Exp)]): Option[Exp] = {
        env.find( _._1 == variable ) match {
            case Some((s, v)) => Some(v)
            case _          => None
        }
    }

    private def indent : Unit = {
        this.indentation += 4
    }

    private def unindent : Unit = {
        assert(this.indentation >= 4)
        indentation -= 4
    }

    private def newLine(s: String) : Unit = {
        print(" " * this.indentation)
        println(s)
    }


    @throws(classOf[ArithmeticException])
    private def compileUnderEnv(ast: Exp, env: List[(String, Exp)]): Unit = ast match {
        // case Var(s) => envLookUp(s, env) match {
        //     case Some(e) => compileUnderEnv(e, env)
        //     case None => throw new BadMUPLExpression("Undefined variable %s".format(s))
        // }
        // // case Munit() => ast
        case Var(s) => {
            newLine("def var():")
            indent
            newLine("return %s".format(s))
            unindent
            newLine("res = var()")
        }
        case Const(i) => {
            newLine("def const():")
            indent
            newLine("return %s".format(i))
            unindent
            newLine("")
            newLine("res = const()")
        }

        // Arithmetic operations.
        case Add(a, b) => (a, b) match {
            case (Var(x), Var(y)) => {
                newLine("def add():")
                indent
                newLine("return %s + %s".format(x, y))
                unindent
                newLine("res = add()")
            }
            case (a, b) => {
                newLine("def add():")
                indent
                compile(a)
                // Trust that res is properly set
                newLine("left_operand = res")
                compile(b)
                newLine("res += left_operand")
                newLine("return res")
                unindent
                newLine("res = add()")
            }
        }

        case Let(name, value, body) => name match {
            case Var(s) => {
                compileUnderEnv(value, env)
                newLine("%s = res".format(s))
                compileUnderEnv(body, env)
            }
            case _      => throw new BadMUPLExpression("Can't compile let expression")
        }

        case _ => print("hi")
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
