package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double]={
        ins match{
            case PushI(d) => {
                d :: stack
            }

            case PopI => {
                stack match{ //Checking the stack
                    case x :: tail => tail
                    case _ => throw new IllegalArgumentException("Error.")
                }
            }

            case AddI => {
                stack match{
                    case x :: y :: tail => (x + y) :: tail
                    case _ => throw new IllegalArgumentException("Error, empty list.")
                }
            }

            case SubI => {
                stack match{
                    case x :: y :: tail => (y - x) :: tail
                    case _ => throw new IllegalArgumentException("Error, empty list.")
                }
            }

            case MultI =>{
                stack match{
                    case x :: y :: tail => (x * y) :: tail
                    case _ => throw new IllegalArgumentException("Error, empty list.")
                }
            }

            case DivI =>{
                stack match{
                    case x :: y :: tail => (y / x) :: tail
                    case _ => throw new IllegalArgumentException("Error, empty list.")
                }
            }

            case LogI =>{
                stack match{
                    case x :: tail if (x > 0) => math.log(x) :: tail
                    case _ => throw new IllegalArgumentException("Error, empty list.")
                }
            }

            case ExpI =>{
                stack match{
                    case x :: tail => math.exp(x) :: tail
                    case _ => throw new IllegalArgumentException("Error, empty list.")
                }
            }

            case SinI => {
                stack match{
                    case x :: tail => math.sin(x) :: tail
                    case _ => throw new IllegalArgumentException("Error, empty list.")
                }
            }

            case CosI => {
                stack match{
                    case x :: tail => math.cos(x) :: tail
                    case _ => throw new IllegalArgumentException("Error, empty list.")
                }
            }
            case _ => throw new IllegalArgumentException("Did not handle.")
        }
    }


    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double =
        instructionList.foldLeft(Nil:List[Double]){
            (acc, currentElement) => emulateSingleInstruction(acc, currentElement)
        }.head


}