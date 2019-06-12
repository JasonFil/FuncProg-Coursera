package week1

import scala.annotation.tailrec

/**
  * A week1 application that shows some edge cases of recursion and call by name.
  * @author <a href="www.jasonfilippou.com">Jason Filippou</a>
  */
object Factorial extends App {

  val ITERS = 100000      // Let's push things

  // Naive Factorial
  def factorialNaive(n:Int) : Int =  if(n == 0) 1 else n * factorialNaive(n-1)

  try {
    for (i <- 1 to ITERS) factorialNaive(i)
    println("Naive factorial worked up to " + ITERS + "!")
  } catch{
    case  se:StackOverflowError => println("Naive factorial threw an instance of StackOverflowError.")
    case e: Exception => println("Naive factorial threw an instance of " + e.getClass + " with message: " + e.getMessage + ".")
  }

  // Tail-recursive factorial
  def factorialTailRec(n: Int) : Int = {
    @tailrec
    def factorialTailRec(n: Int, f: => Int): Int = {
      if (n == 0) {
        def fEvaluated = f
        fEvaluated
      }
      else {
        def fEvaluated = f
        factorialTailRec(n - 1, n * fEvaluated)
      }
    }
    factorialTailRec(n, 1)
  }

  try {
    for(i <-1 to ITERS) factorialTailRec(i)
    println("Tail recursive factorial worked up to " + ITERS + "!")
  } catch{
      case  se:StackOverflowError => println("Tail recursive factorial threw an instance of StackOverflowError.")
      case e: Exception => println("Tail recursive factorial threw an instance of " + e.getClass + " with message: " + e.getMessage + ".")
  }

  println("Exiting...")

}
