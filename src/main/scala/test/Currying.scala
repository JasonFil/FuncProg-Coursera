package test

import scala.annotation.tailrec

object Currying extends App{

  def sum1(f:Int=>Int)(a:Int, b:Int): (Int, Int) => Int = {
    require(a <=  b, "Received a=" + a + " and b=" + b)
    @tailrec
    def sum(a:Int, acc:Int) : Int = {
      if(a > b) acc
      else sum(a+1, acc + f(a))
    }
    sum
  }

  //println("sum1(x=>x*x)(1, 4)=" + sum1(x=>x*x)(1, 4))
  println("sum1(x=>x*x)(1, 4)=" + sumSq(1, 4))
  println("sum1(x=>x*x*x)(1, 4)=" + sum1(x=>x*x*x)(1, 4))

  def sum2(f:Int=>Int)(a:Int, b:Int): Int => Int = {
    def sum(a:Int) : Int = {
      if(a > b) 0
      else f(a) + sum(a+1)
    }
    sum
  }

  println("sum2(x=>x*x)(1, 4)=" + sum2(x=>x*x)(1, 4))
  println("sum2(x=>x*x*x)(1, 4)=" + sum2(x=>x*x*x)(1, 4))

  def givenSum(f:Int=>Int):(Int, Int) => Int = {
    def sumF(a:Int, b:Int) : Int = {
      if(a > b) 0
      else f(a) + sumF(a+1, b)
    }
    sumF
  }
  println("hi")
  println("Sum of squares from 1 through 10 is: " + givenSum(x=>x*x)(1, 10))

  def sumSq = givenSum(x=>x*x)

  println("Sum of squares from 1 through 10 is: " + sumSq(1, 10))
}
