package week2.rational

import scala.annotation.tailrec

object Runner extends App {

  def pow(base:Int, exp:Int):Int = {

      require(exp >= 0 && base >=0, "We need positive integer arguments for this method.")

      // Power computation with tail-recursive repeated squaring!
      @tailrec
      def pow(currExp:Int, maxExp:Int, currTerm:Int, prodAccum:Int): Int = {
        assert(currExp <= maxExp, "The current exponent should never surpass the original one.")
        if(currExp <= maxExp / 2)
          pow(2*currExp, maxExp, currTerm * currTerm, prodAccum) // Next iteration
        else if (currExp == maxExp) currTerm  * prodAccum                   // Bottomed out.
        else  pow(1, maxExp - currExp, base, currTerm * prodAccum) // Compute residual product using the same method.
      }

      if (base == 0 && exp == 0) throw new IllegalArgumentException("0^0 is an undefined form.")
      else if(base == 0 && exp > 0) 0
      else if(base > 0 && exp == 0) 1
      else if(exp == 1) base
      else if(base == 1) 1
      else pow(1, exp, base, 1)
    }

  // Make sure we throw when asking for 0^0
  try {
    println(pow(0, 0))
    throw new RuntimeException("Computation of 0^0 should have thrown an instance of IllegalArgumentException.")
  } catch {
    case _:IllegalArgumentException => println("0^0 is undefined.")
    case e: Exception => throw e
  }

  // Test
  for(i<-0 to 10)
    for(j<-1 to 10)
      println(i+"^"+j + "=" + pow(i, j))

}
