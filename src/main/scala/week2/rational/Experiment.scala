package week2.rational

import scala.util.Random

/**
  * Stress-test app for the type `Rational`.
  * @see Rational
  */
object Experiment extends App{

  // Timing method, courtesy of http://biercoff.com/easily-measuring-code-execution-time-in-scala/
  def time[R](block: => R, msg:String): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time for " + msg + " was " +  (t1 - t0) + "ns")
    result
  }

  println("======= EXPERIMENT 1: ACCURACY AND EFFICIENCY OF STANDARD MAP-REDUCE ========== ");

  final val rng = new Random(47)
  final val MAX_ITER = 1000
  final val MAX_INT = 50   // Keep the numbers small for this exposition.
  val intTupleSeq : Seq[(Int, Int)]= for (_ <- 1 to MAX_ITER) yield (rng.nextInt(MAX_INT) + 1, rng.nextInt(MAX_INT) + 1)
  val quotientSeq : Seq[Double] = intTupleSeq map { case (a, b) => a / b.toDouble }
  val rationalSeq : Seq[Rational] = intTupleSeq map { case (a, b) => new Rational(a, b) }

  // Sums first....
  val quotientSum = time( quotientSeq.sum, "quotient sum")
  val rationalSum = time(rationalSeq.reduce((x, y) => x+y), "Rational sum")
  println("Value of quotient sum:" + quotientSum)
  val evaluatedRationalSum = rationalSum.eval
  println("Value of Rational sum:" + evaluatedRationalSum)
  println("Error: " + Math.abs(quotientSum - evaluatedRationalSum))

  // Products second...
  val quotientProd = time( quotientSeq.product, "quotient product")
  val rationalProd = time(rationalSeq.reduce((x, y) => x*y), "Rational product")
  println("Value of quotient product:" + quotientProd)
  val evaluatedRationalProd = rationalProd.eval
  println("Value of Rational product:" + evaluatedRationalProd)
  println("Error: " + Math.abs(quotientProd - evaluatedRationalProd))
  
  println("=========== EXPERIMENT 2: ACCURACY AND EFFICIENCY OF EXPONENTIATION ========= ")
  final val EXPONENT = 200
  val randomIdx = rng.nextInt(intTupleSeq.length)
  var quotientPower: Double = 0.0
  var rationalPower : Rational= _
  time(quotientPower = scala.math.pow(quotientSeq(randomIdx), EXPONENT), "Double raised to the power of " + EXPONENT)
  time(rationalPower = rationalSeq(randomIdx) ^ EXPONENT, "Rational raised to the power of " + EXPONENT)
  println("Value of power from Doubles: " + quotientPower + ".")
  println("Value of power from Rationals" + rationalPower.eval + ".")
  println("Error: " + Math.abs(quotientPower - rationalPower.eval))
}
