package week2.rational

import scala.util.Random

/**
  * Stress-test app for the type `Rational`.
  * @see Rational
  */
object Experiment extends App{

  def time[R](block: => R, msg:String): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time for " + msg + " was " +  (t1 - t0) + "ns")
    result
  }

  val rng = new Random(47)
  val MAX_ITER = 100000
  val MAX_INT = 1000   // Keep the numbers small for this exposition.
  val intTupleSeq : Seq[(Int, Int)]= for (_ <- 1 to MAX_ITER) yield (rng.nextInt(MAX_INT) + 1, rng.nextInt(MAX_INT) + 1)
  val quotientSeq : Seq[Double] = intTupleSeq map { case (a, b) => a / b.toDouble }
  val rationalSeq : Seq[Rational] = intTupleSeq map { case (a, b) => new Rational(a, b) }
  //println(intTupleSeq)
  //println(rationalSeq)
  val quotientReduction = time( quotientSeq.sum, "quotient reduction")
  val rationalReduction = time(rationalSeq.reduce((x, y) => x+y), "Rational reduction")
  println("Value of reduction from Doubles:" + quotientReduction)
  val evaluatedRationalReduction = rationalReduction.eval
  println("Value of reduction from Rationals:" + rationalReduction + "=\n\t= " + evaluatedRationalReduction)
  println("Error: " + Math.abs(quotientReduction - evaluatedRationalReduction))

  val EXPONENT = 1024
  println("Evaluate power accuracy and efficiency:")
  val randomIdx = rng.nextInt(intTupleSeq.length)
  var quotientPower: Double = 0.0
  var rationalPower : Rational= _
  time(quotientPower = scala.math.pow(quotientSeq(randomIdx), EXPONENT), "Double raised to the power of " + EXPONENT)
  time(rationalPower = rationalSeq(randomIdx) ^ EXPONENT, "Rational raised to the power of " + EXPONENT)
  println("Value of power from Doubles: " + quotientPower + ".")
  println("Value of power from Rationals" + rationalPower.eval + ".")
}
