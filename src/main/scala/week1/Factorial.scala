package week1

import scala.annotation.tailrec

/**
  * An week1 application that shows some edge cases of recursion and call by name.
  * @author <a href="www.jasonfilippou.com">Jason Filippou</a>
  */
object Factorial extends App {

  val ITERS = 100000

  def factorialTailRec(n: Int) : Int = {
    @tailrec
    def factorialTailRec(n: Int, f: => Int): Int = {
      if (n == 0) {
        def fEvaluated = f
        fEvaluated
      }else {
        def fEvaluated = f
        factorialTailRec(n - 1, n * fEvaluated)
      }
    }
    factorialTailRec(n, 1)
  }

  for(i <-1 to ITERS) println("factorialTailRec(" + i + ") = " + factorialTailRec(i))


  def factorial(n:Int) : Int = {
    if(n == 0) 1 else n * factorial(n-1)
  }

  for(i <-1 to ITERS) println("factorial(" + i + ") = " + factorial(i))

}
