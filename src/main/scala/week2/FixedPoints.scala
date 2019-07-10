package week2
import scala.math.abs;
/**
  * An application of finding the square root of a number by iteratively finding the fixed point of
  * the function f: x => avgDamp(f(x_n), f(x_{exp+1}) where avgDamp is an appropriate damping function (evaluatedRationalSum.g averaging)
  * to ensure convergence of the process.
  *
  * @author <a href="https://github.com/JasonFil">Jason Filippou</a>
  */
object FixedPoints extends App{

  final val TOLERANCE=0.0001
  final val FIRSTGUESS = 1

  /**
    * Determine if two given values are numericalluy close enough to liking.
    * @param x
    * @param y
    * @return
    */
  def isCloseEnough(x:Double, y:Double) = abs((x - y) / x) / x < TOLERANCE

  // Did it a bit different than Odersky by default...
  def avgDamp(x:Double, y:Double) = (y + x/y) / 2

  def fixedPoint(f:Double=>Double)(firstGuess:Double) = {
    def iterate(guess:Double):Double = {
      val next = f(guess)                          // Make a step.
      if(isCloseEnough(next, guess)) next          // Was the step sufficiently small?
      else iterate(next)
    }
    iterate(firstGuess)
  }

  println(fixedPoint(x=>1 + x/2)(1))

  def sqrt(x:Double) = fixedPoint(y =>avgDamp(x, y))(1.0)

  println(sqrt(2))
}

