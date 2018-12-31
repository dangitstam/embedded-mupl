package EmbeddedMUPL.Compiler

import EmbeddedMUPL.Language.MUPL._


object Compiler {

    var indentation = 0

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

    /**
     * Interprets the AST to a value (Const / Munit) beginning from an
     * empty environment.
     */
    @throws(classOf[ArithmeticException])
    def compile(ast: Exp): Unit = ast match {
        // // case Munit() => ast
        case Var(s) => {
            newLine("res = %s".format(s))
        }
        case Const(i) => {
            newLine("res = %s".format(i))
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
                compile(value)
                newLine("%s = res".format(s))
                compile(body)
            }
            case _      => throw new BadMUPLExpression("Let: invalid variable name")
        }
        case Fun(v1: Var, v2: Var, e) => {
            // TODO: Anonymous and named cases.
            val Var(name) = v1; val Var(arg) = v2
            newLine("def %s(%s):".format(name, arg))
            indent
            compile(e)
            newLine("return res")
            unindent
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
