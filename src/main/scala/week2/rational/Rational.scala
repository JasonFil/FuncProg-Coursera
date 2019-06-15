package week2.rational
import com.sun.javaws.exceptions.InvalidArgumentException

import scala.annotation.tailrec

/**
  * Immutable rational number abstraction. Offers standard rational arithmetic operations.
  * @param x The numerator of the fraction.
  * @param y The denominator of the fraction.
  */
class Rational (x:Int, y:Int){

  import Rational._

  require(y != 0, "Cannot create a rational with denominator 0")

  // Simplify representation by dividing both numer and denom by their gcd.
  private def gcd(a:Int, b:Int) : Int = if(b == 0) a else gcd(b, a % b)
  private val gcd : Int= gcd(x, y)
  private val numer = x / gcd // Make immutable
  private val denom = y / gcd


  /**
    * Compute the rational analytically.
    * @return numer / denom
    */
  def  compute:Double = x/y
  /**
    * Creates the rational x/1.
    * @param x The numerator of the new Rational.
    */
  def this(x:Int) = this(x, 1)

  override def toString() : String = numer + "/" + denom // give me something to work with

  // Properly writing an equals() method in Scala can be tricky business, as explained here: ]
  // https://alvinalexander.com/scala/how-to-define-equals-hashcode-methods-in-scala-object-equality
  private def canEqual(x:Any) = x.isInstanceOf[Rational]

  private def sameRational(that:Rational): Boolean = {
    (numer == that.numer) && (denom == that.denom)
  }

  override def equals(that:Any) = {
    that match {
      case that:Rational => that.canEqual(this)  && sameRational(that)// The pre-condition ensures that`that` is an instance of Rational while the post-condition tests if `this` is an instance of `that`.
      case _ => false // covers null too (check http://daily-scala.blogspot.com/2010/01/matching-nulls.html)

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

  /**
    * Adds the provided Rational instance to this.
    * @param that The provided Rational.
    * @return this + that.
    */
  def add(that:Rational):Rational = {
    require(that != null, "Rational:add(Rational, Rational): Provided null argument.")
    new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)
  }

  /**
    * Subtracts the provided Rational instance from this.
    * @param that A Rational instance, which will be the subtrahend.
    * @return this - that.
    */
  def subtract(that:Rational):Rational = {
    require(that != null, "Rational:subtract(Rational, Rational): Provided null argument.")
    add(that.neg)
  }

  /**
    * Multiples the provided Rational instance with this.
    * @param that A Rational instance.
    * @return this * that.
    */
  def multiply(that:Rational):Rational = {
    require(that != null, "Rational::multiply(Rational): Provided null argument")
    new Rational(this.numer * that.numer, this.denom * that.denom)
  }

  /**
    * Divides the current Rational instance over the provided Rational instance,
    * as long as it is non-zero.
    * @param that A Rational instance which will play the role of the divisor.
    * @return this / that.
    * @throws InvalidArgumentException if that is zero.
    */
  def divide(that: Rational): Rational = {
    require((that != null) && that.isZero, "Rational::divide(Rational): Provided null or zero argument")
    multiply(that invert)
  }


  /**
    * Negates this.
    * @return -this.
    */
  def neg = new Rational(-numer, denom)

  /**
    * Inverts this, as long as it is non-zero.
    * @return 1/this.
    */
  def invert: Rational = {
    require(numer != 0, "Rational::invert(): Cannot invert a Rational with a numerator of zero.")
    new Rational(denom, numer)
  }

  /**
    * Inverts *and* negates this.
    * @return - 1 / this.
    */
  def negInvert:Rational = this.neg.invert

  /**
    * Checks if the current rational is zero. Denominator does not matter.
    * @return true if this is zero, false otherwise.
    */
  def isZero:Boolean = numer == 0

  /**
    * Checks if the current rational is STRICTLY greater than zero.
    * @return true if this is greater than zero, false otherwise.
    */
  def gtZero:Boolean = (numer > 0) && (denom > 0) || (numer < 0) && (denom < 0)

  /**
    * Checks if the current rational is equal to OR greater than zero.
    * @return true iff this >= 0, else false.
    */
  def geqZero:Boolean = isZero || gtZero

  /**
    * Checks if the current rational is STRICTLY smaller than zero.
    * @return true iff this is negative, false otherwise.
    */
  def ltZero:Boolean = !geqZero

  /**
    * Checks of the current rational is smaller than OR equal to ZERO
    * @return true iff this is zero or negative.
    */
  def leqZero: Boolean = ltZero || isZero

  /**
    * Returns the maximum between this and that.
    * @param that A Rational instance to compare against this.
    * @return this, iff this >= that, that otherwise.
    */
  def max(that:Rational):Rational = {
      if(that.subtract(this).gtZero) that
      else this
  }

  /**
    * Returns the minimum between this and that.
    * @param that A Rational instance to compare against this.
    * @return that, if this >= that, otherwise this.
    */
  def min(that:Rational):Rational = if(this.max(that)  == this) that else this

  /**
    * Raises this to the provided power. The implementation is tail recursive
    * and through repeated squaring, such that the code executes in \ceil{log_2n} iterations.
    * @param n the power to which we will raise the current Rational instance.
    * @return (this)^n
    */
  def pow(n:Int):Rational = {
    // Inner two-arg function
    def pow(base:Int, exp:Int):Int = {

      require(exp >= 0 && base >=0, "We need positive integer arguments for this method.")
      // Power computation with tail-recursive repeated squaring.
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

    if(n == 1) this
    else if(n == 0) ONEOVERONE
    else if(n < 0) new Rational(pow(denom, -n), pow(numer, -n))
    else new Rational(pow(numer, n), pow(denom, n))
  }

}

/**
  * Companion object of class Rational.
  */
object Rational{

  /**
    * A Rational instance which represents 1/1.
    */
  val ONEOVERONE = new Rational(1, 1)

  /**
    * * A Rational instance which represents  0/1.
    */
  val ZEROVERONE = new Rational(0, 1)

  /**
    ** A Rational instance which represents  (-1)/1.
    */
  val MINUSONEOVERONE = new Rational(-1, 1)

  /**
    ** A Rational instance which represents 1/(-1).
    */
  val ONEOVERMINUSONE = new Rational(1, -1)

  /**
    * Static version of Rational::add.
    * @param r1 A Rational instance.
    * @param r2 A Rational instance.
    * @return r1 + r2.
    */
  def add(r1:Rational, r2:Rational):Rational = {
      require((r1 != null) && (r2 != null), "Rational:add(Rational, Rational): Provided null argument.")
      r1.add(r2)
  }

  /**
    * Static version of Rational::subtract.
    * @param r1 A Rational instance.
    * @param r2 A Rational instance.
    * @return r1 - r2.
    */
  def subtract(r1:Rational, r2:Rational):Rational = {
    require((r1 != null) && (r2 != null), "Rational:subtract(Rational, Rational): Provided null argument.")
    r1.subtract(r2)
  }

  /**
    * Static version of Rational::multiply.
    * @param r1 A Rational instance.
    * @param r2 A Rational instance.
    * @return r1 * r2.
    */
  def multiply(r1:Rational, r2:Rational):Rational = {
    require((r1 != null) && (r2 != null), "Rational:multiply(Rational, Rational): Provided null argument.")
    r1.multiply(r2)
  }

  /**
    * Static version of Rational::divide.
    * @param r1 A Rational instance.
    * @param r2 A Rational instance.
    * @return r1 / r2.
    */
  def divide(r1:Rational, r2:Rational):Rational = {
    require((r1 != null) && (r2 != null), "Rational:divide(Rational, Rational): Provided null argument.")
    r1.multiply(r2)
  }


  /**
    * Static version of Rational::max.
    * @param r1 A Rational instance.
    * @param r2 A Rational instance.
    * @return The maximum rational between r1 and r2.
    */
  def max(r1:Rational, r2:Rational) = {
    require((r1 != null) && (r2 != null), "Rational:max(Rational, Rational): Provided null argument.")
    r1.max(r2)
  }

  /**
    *
    * @param r1
    * @param r2
    * @return
    */
  def min(r1:Rational, r2:Rational) : Rational = {
    require((r1 != null) && (r2 != null), "Rational:min(Rational, Rational): Provided null argument.")
    r1.min(r2)
  }

}