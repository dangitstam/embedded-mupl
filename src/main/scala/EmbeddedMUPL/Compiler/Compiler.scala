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
     * Construct a generalized binary operator.
     */
    def binop(a: Exp, b: Exp, op: String): Unit = (a, b) match {
        case (Var(x), Var(y)) => {
            newLine("res = %s %s %s".format(x, op, y))
        }
        case (Var(x), b) => {
            // Compile y, add it to x
            newLine("def binop():")
            indent; compile(b)
            newLine("return %s %s res".format(x, op))
            unindent; newLine("res = binop()")
        }
        case (a, Var(x)) => {
            // Binary operations are commutative for arithmetic.
            // If ">" is being used, substitute the operation with "<".
            if (op == ">") {
                binop(Var(x), a, "<")
            } else {
                binop(Var(x), a, op)
            }   
        }
        case (a, b) => {
            newLine("def binop():")
            indent; compile(a)
            newLine("left_operand = res")
            compile(b)
            newLine("res = left_operand %s res".format(op))
            newLine("return res")
            unindent; newLine("res = binop()")
        }
    }

    /**
     * Compiles the AST to valid Python 3 code.
     */
    @throws(classOf[ArithmeticException])
    def compile(ast: Exp): Unit = ast match {
        case Munit() => newLine("res = null")
        case Var(s) => newLine("res = %s".format(s))
        case Const(i) => newLine("res = %s".format(i))
        case Add(a, b) => binop(a, b, "+")
        case Subtract(a, b) => binop(a, b, "-")
        case Multiply(a, b) => binop(a, b, "*")
        case Divide(a, b) => binop(a, b, "/")
        case IsGreater(a, b) => {
            binop(a, b, ">")
            newLine("res = int(res)")
        }
        case Ifnz(cond, e1, e2) => {
            newLine("def true_branch():")
            indent; compile(e1)
            newLine("return res")
            unindent
            newLine("def false_branch():")
            indent; compile(e2)
            newLine("return res")
            unindent; compile(cond)
            newLine("res = true_branch() if res != 0 else false_branch()")
        }
        case Let(name, value, body) => name match {
            case Var(s) => {
                compile(value)
                newLine("%s = res".format(s))
                compile(body)
            }
            case _    => throw new BadMUPLExpression("Let: invalid variable name")
        }

        // Function call semantics.
        case Call(fn: Exp, arg: Exp) => {
            // Bind a value to the compiled argument.
            newLine("def call():")
            indent; compile(arg)
            newLine("arg = res")
            fn match {
                // Function call uses a reference
                case Var(name) => {
                    newLine("res = $s(arg)".format(name))
                }
                // Function is a value.
                case Fun(v1, v2, e) => {
                    compile(fn)
                    v1 match {
                        case Var(name) => newLine("res = $s(arg)".format(name))

                        // Anonymous functions will bind `null` to the lambda.
                        case _         => newLine("res = null(arg)")
                    }
                }
                case _ => {
                    throw new BadMUPLExpression("Function call applied to non-function")
                }
            }
            newLine("return res")
            unindent; newLine("res = call()")
        }
        case Fun(v1, v2: Var, e) => {
            val Var(arg) = v2
            v1 match {
                case Var(name) => {
                    newLine("def %s(%s):".format(name, arg))
                    indent; compile(e)
                    newLine("return res")
                    unindent
                }
                case null => {
                    // Anonymous functions.
                    // `null` is a keyword in Scala but not in Python!
                    newLine("def null(arg):")
                    indent; compile(e)
                    newLine("return res")
                    unindent
                }
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
