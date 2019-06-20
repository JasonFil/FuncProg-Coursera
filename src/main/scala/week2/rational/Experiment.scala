package week2.rational
import scala.util.Random

object Experiment extends App{
  val rng = new Random(47)
  val MAX_ITER = 1000
  val MAX_INT = 5      // Keep the numbers small for this exposition.
  for(_ <-1 to MAX_ITER) {
    val a = rng.nextInt(MAX_INT) + 1
    if(a == 0) throw new RuntimeException("Somehow generated a 0")
  }
  val intTupleSeq : Seq[(Int, Int)]= for (_ <- 1 to MAX_ITER) yield (rng.nextInt(MAX_INT) + 1, rng.nextInt(MAX_INT) + 1)
  val quotientSeq : Seq[Double] = intTupleSeq map { case (a, b) => a / b.toDouble }
  val rationalSeq : Seq[Rational] = intTupleSeq map { case (a, b) => new Rational(a, b) }
  //println(intTupleSeq)
  //println(rationalSeq)
  val quotientProd = quotientSeq.sum
  val rationalProd = rationalSeq.reduce((x, y) => x+y).eval
  println(quotientProd)
  println(rationalProd)
}
