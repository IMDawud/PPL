package edu.colorado.csci3155.project1

object StackMachineCompiler {

    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */

    /*
    case class Const(f: Double) extends  Expr
    case class Plus(e1: Expr, e2: Expr) extends  Expr
    case class Minus(e1: Expr, e2: Expr) extends  Expr
    case class Mult(e1: Expr, e2: Expr) extends  Expr
    case class Div(e1: Expr, e2: Expr) extends  Expr
    case class Exp(e:Expr) extends Expr
    case class Log(e: Expr) extends Expr
    case class Sine(e: Expr) extends Expr
    case class Cosine(e:Expr) extends Expr
     */

    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match {
            case Const(f) => {
                List(PushI(f))
            }

            case Plus(e1, e2) => {
                val L1 = compileToStackMachineCode(e1)
                val L2 = compileToStackMachineCode(e2)
                L1 ::: L2 ::: List(AddI)
            }

            case Minus(e1, e2) => {
                val L1 = compileToStackMachineCode(e1)
                val L2 = compileToStackMachineCode(e2)
                L1 ::: L2 ::: List(SubI)
            }

            case Mult(e1, e2) => {
                val L1 = compileToStackMachineCode(e1)
                val L2 = compileToStackMachineCode(e2)
                L1 ::: L2 ::: List(MultI)
            }

            case Div(e1, e2) => {
                val L1 = compileToStackMachineCode(e1)
                val L2 = compileToStackMachineCode(e2)
                L1 ::: L2 ::: List(DivI)
            }

            case Exp(e1) => {
                val L1 = compileToStackMachineCode(e1)
                L1 ::: List(ExpI)
            }

            case Log(e1) => {
                val L1 = compileToStackMachineCode(e1)
                L1 ::: List(LogI)
            }

            case Sine(e1) => {
                val L1 = compileToStackMachineCode(e1)
                L1 ::: List(SinI)
            }

            case Cosine(e1) => {
                val L1 = compileToStackMachineCode(e1)
                L1 ::: List(CosI)
            }

        }

    }
}
