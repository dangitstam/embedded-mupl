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
     * Compiles the AST to valid Python 3 code.
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
        // TODO: Can add be generalized?
        case Add(a, b) => (a, b) match {
            case (Var(x), Var(y)) => {
                newLine("res = %s + %s".format(x, y))
            }
            case (Var(x), b) => {
                // Compile y, add it to x
                newLine("def add():")
                indent
                compile(b)
                newLine("return %s + res".format(x))
                unindent
                newLine("res = add()")
            }
            case (a, Var(x)) => {
                // Add is commutative.
                compile(Add(Var(x), a))
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
        case Ifnz(cond, e1, e2) => {
            newLine("def true_branch():")
            indent
            compile(e1)
            newLine("return res")
            unindent
            newLine("def false_branch():")
            indent
            compile(e2)
            newLine("return res")
            unindent
            compile(cond)
            newLine("res = true_branch() if res == 0 else false_branch()")
        }
        case Let(name, value, body) => name match {
            case Var(s) => {
                compile(value)
                newLine("%s = res".format(s))
                compile(body)
            }
            case _  => throw new BadMUPLExpression("Let: invalid variable name")
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
