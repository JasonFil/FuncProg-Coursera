package week2

import scala.annotation.tailrec

object HigherOrderFunctions extends App{

    def sum(f: Int=>Int, a:Int, b:Int) = { // Return type inference]]

      @tailrec
      def sumRec(a:Int, acc:Int): Int = {
        if(a > b) acc
        else sumRec(a+1, f(a) + acc)
      }
      sumRec(a, 0)
    }

    println(sum(x=>x*x, 1, 40000000))
    println(sum(x=>x*x*x, 1, 4000000))
}
