package week2

import scala.annotation.tailrec

object Currying extends App{

  /**
    * Allows for the calculation of the sum of the given function application on a range of provided ints.
    * Requires that the interval provided is ordered.
    * @param f The Int=>Int function which will be applied on the individual Ints.
    * @param a The lower end of the range.
    * @param b The upper end of the range.
    * @note First argument needs to be not greater than the second one.
    * @return A function which will calculate the sum of f(i) for all Ints i in the closed interval [a, b]
    */
 /* def sum(f:Int=>Int)(a:Int, b:Int): Int = {
    require(a <=  b, "Received a=" + a + " and b=" + b)
    @tailrec
    def sum(a:Int, acc:Int) : Int = {
      if(a > b) acc
      else sum(a+1, acc + f(a))
    }
    sum(a, 0)
  }

  println("Sum of squares from 1 to 100 = " + sum(x=>x*x)(1, 100))
  println("Sum of cubes from 1 to 100 = " + sum(x=>x*x*x)(1, 100))*/


  /**
    * Calculates the product of the given function application on a range of provided ints. Requires that the interval provided is ordered.
    * @param f The Int=>Int function which will be applied on the individual Ints. The function is tail-recursive.
    * @param a The lower end of the range.
    * @param b The upper end of the range.
    * @note First argument needs to be not greater than the second one.
    * @return The product of the applications of f on the integers in [a, b].
    */
  def product(f:Int=>Int)(a:Int, b:Int):Int= {
    require(a <=  b, "Received a=" + a + " and b=" + b)
    @tailrec
    def product(a:Int, acc:Int) : Int = {
      if(a > b) acc
      else product(a+1, acc * f(a))
    }
    product(a, 1)
  }

  println("Product of squares from 1 to 100 = " + product(x=>x*x)(1, 100))
  println("Product of cubes from 1 to 100 = " + product(x=>x*x*x)(1, 100))

  /**
    * Simple tail-recursive factorial.
    * @param n The non-negative integer to calculate the factorial of.
    * @return exp!
    */
  def factorial(n:Int) = {
    require(n >=0, "Provided invalid quantity for exp: " + n)
    product(x=>x)(1, n)
  }

  /**
    * A generalized map-reduce framework on a closed interval of Ints.
    * @param f a unary operator to apply on all interval elements.
    * @param op a binary operator to pairwise reduce the interval elements to a single number.
    * @param neutral The neutral element of op. For example, neutral=1 if op = '*'.
    * @param lb The inclusive lower bound of the interval.
    * @param ub The inclusive upper bound of the interval.
    */
  def mapReduce(f:Int=>Int, op: (Int, Int)=>Int, neutral:Int)(lb:Int, ub:Int) = {
    require(lb <= ub, "Invalid bounds given; lb=" + lb + ", ub = " + ub + ".")
    def mapReduce(a:Int, acc:Int):Int = {
      if(a > ub) acc
      else mapReduce(a+1, op(f(a), acc))
    }
    mapReduce(lb, neutral)
  }
}
