package week2.rational
import scala.annotation.tailrec

/**
  * Immutable rational number abstraction.
  * @param x
  * @param y
  */
class Rational (x:Int, y:Int){

  require(y != 0, "Cannot create a rational with denominator 0")

  val ONEOVERONE = new Rational(1, 1)
  val ZEROVERONE = new Rational(0, 1)
  val MINUSONEOVERONE = new Rational(-1, 1)
  val ONEOVERMINUSONE = new Rational(1, -1)

  // Simplify representation by dividing both numer and denom by their gcd.
  private def gcd(a:Int, b:Int) : Int = if(b == 0) a else gcd(b, a % b)
  private val gcd : Int= gcd(x, y)
  private val numer = x / gcd // Make immutable
  private val denom = y / gcd

  override def toString() : String = numer + "/" + denom // give me something to work with

  // Properly writing an equals() method in Scala can be tricky business, as explained here: ]
  // https://alvinalexander.com/scala/how-to-define-equals-hashcode-methods-in-scala-object-equality
  private def canEqual(x:Any) = x.isInstanceOf[Rational]

  override def equals(that:Any) = {
    that match {
      case that:Rational => that.canEqual(this)  && numer == that.numer && denom == that.denom// The pre-condition ensures that`that` is an instance of Rational while the post-condition tests if `this` is an instance of `that`.
      case _ => false

    }
  }

  // Give a new hashCode since I overrode equals
  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + numer
    result = prime*result + denom
    result
  }
  def add(that:Rational) = new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

  def neg = new Rational(-numer, denom)

  def subtract(that:Rational) = add(that.neg)

  def multiply(that:Rational) = new Rational(this.numer * that.numer, this.denom * that.denom)

  def divide(that:Rational) = {
    new Rational(this.numer * that.denom, this.denom * that.numer )
  }

  def lessThan(that:Rational): Unit = {
    def ltZero(this.subtract(that))
  }

  def pow(n:Int):Rational = {
    // Inner two-arg function
    def pow(base:Int, exp:Int):Int = {
      // Power computation with tail-recursive repeated squaring!
      @tailrec
      def pow(currExp:Int, maxExp:Int, currTerm:Int, prodAccum:Int): Int = {
        if(currExp < maxExp) pow(2*currExp, maxExp, currTerm * currTerm, prodAccum) // Next iteration
        else if (currExp == maxExp) currTerm  * prodAccum                   // Bottomed out.
        else  pow(1, maxExp - currExp, base, prodAccum * currTerm ) // Compute residual product using the same method
      }
      pow(1, exp, base, 1)
    }
    if(n == 1) this
    else if(n == 0) ONEOVERONE
    else new Rational(pow(numer, n), pow(denom, n))
  }

}
