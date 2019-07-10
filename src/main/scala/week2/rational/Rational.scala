package week2.rational
import scala.annotation.tailrec

/**
  * Immutable rational number abstraction. Overloads standard arithmetic operators for transparency.
  * Implements exponentiation using tail-recursive repeated squaring. Inner representation is as a tuple
  * of `BigInt` instances. Several operators are overloaded for binary operations such as addition
  * and multiplication, as well as binary comparison operators. A method for exponentiation based on
  * repeated squaring is also provided.
  * @param x The numerator of the fraction.
  * @param y The denominator of the fraction.
  *
  * @author <a href="https://github.com/JasonFil">Jason Filippou</a>
  */
class Rational(x:BigInt, y:BigInt) {

  import Rational._   // Companion object has all our constants

  require(y != 0, "Cannot create a Rational with denominator 0")

  // Simplify representation by dividing both numer and denom by their gcd.
  @tailrec
  private def gcd(a:BigInt, b:BigInt) : BigInt = if(b == 0) a else gcd(b, a % b)
  private val gcd : BigInt= gcd(x, y)
  private val numer = x / gcd // Make immutable
  private val denom = y / gcd


  /**
    * Evaluate the `Rational` analytically.
    * @return numer / denom
    */
  def  eval: Double = numer.doubleValue() /  denom.doubleValue()

  /**
    * Creates the `Rational` x/1.
    * @param x The numerator of the new `Rational`.
    */
  def this(x:BigInt) = this(x, 1)

  /**
    * Prints `this` in the form of the fraction `numer` / `denom`.
    * @return The fraction `numer` / `denom` (the `Rational` instance unevaluated).
    */
  override def toString : String = numer + "/" + denom // A nice visual, "unevaluated" format

  // Properly writing an equals() method in Scala can be tricky business, as explained here: ]
  // https://alvinalexander.com/scala/how-to-define-equals-hashcode-methods-in-scala-object-equality
  private def canEqual(x:Any):Boolean = x.isInstanceOf[Rational]

  private def sameRational(that:Rational ): Boolean =
    (numer == that.numer) && (denom == that.denom) ||
      (numer == -that.numer) && (denom == -that.denom)

  override def equals(that:Any):Boolean = {
    that match {
      case that:Rational => that.canEqual(this)  && sameRational(that)// The pre-condition ensures that`that` is an instance of Rational while the post-condition tests if `this` is an instance of `that`.
      case that:BigInt => sameRational(new Rational(that, 1))      // When we compare with integers.
      case _ => false // covers null too (check http://daily-scala.blogspot.com/2010/01/matching-nulls.html)
    }
  }

  // Give a new hashCode since I override equals
  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + numer.intValue
    result = prime * result + denom.intValue
    result
  }

  /**
    * Adds the provided `Rational` instance to `this`.
    * @param that The provided `Rational`.
    * @return this + that.
    */
  def + (that:Rational):Rational  = {
    require(that != null, "Rational + Rational: Provided null argument.")
    new Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)
  }

  /**
    * Add `this` to the provided `BigInt`.
    * @param that An `BigInt` to add `this` to.
    * @return The `Rational` instance corresponding to the fraction (this + that).
    */
  def + (that:BigInt): Rational = this + new Rational(that, 1)

  /**
    * Subtracts the provided `Rational` instance from `this`.
    * @param that A `Rational` instance, which will be the subtrahend.
    * @return this - that.
    */
  def - (that:Rational):Rational  = {
    require(that != null, "Rational - Rational: Provided null argument.")
    this + that.unary_-
  }

  /**
    * Subtracts the `BigInt` instance `that` from `this`.
    * @param that An `BigInt` instance to subtract from `this`.
    * @return this - that.
    */
  def - (that:BigInt) : Rational =  this - new Rational(that, 1)

  /**
    * Multiplies the provided `Rational` instance with `this`.
    * @param that A `Rational` instance.
    * @return this * that.
    */
  def * (that:Rational):Rational = {
    require(that != null, "Rational * Rational: Provided null argument")
    new Rational(this.numer * that.numer, this.denom * that.denom)
  }

  /**
    * Multiplies the provided `BigInt` instance with `this`.
    * @param that A Rational instance.
    * @return this * that.
    */
  def * (that:BigInt) = new Rational(numer * that, denom)

  /**
    * Divides `this` instance over the provided `Rational` instance,
    * as long as it is non-zero.
    * @param that A `Rational` instance which will play the role of the divisor.
    * @return this / that.
    * @throws IllegalArgumentException if `that` is zero.
    */
  def / (that: Rational ): Rational = {
    //noinspection ComparingUnrelatedTypes // for that != 0
    require((that != null) && that != 0, "Rational / Rational: Provided null or zero argument")
    this * (that ^ (-1))
  }

  /**
    * Divides `this` instance over the provided `BigInt` instance,
    * as long as it is non-zero.
    * @param that A `Rational` instance which will play the role of the divisor.
    * @return this / that.
    * @throws IllegalArgumentException if `that` is zero.
    */
  def / (that:BigInt) : Rational = {
    //noinspection ComparingUnrelatedTypes
    require((that != null) && that != 0, "Rational / Rational: Provided null or zero argument")
    new Rational(numer, denom * that)
  }
  
  /**
    * Negates `this`.
    * @return -this.
    */
  def unary_- : Rational = new Rational (-numer, denom)

  /**
    * Checks if `this` is STRICTLY greater than the given `Rational`.
    * @return true iff `this` is greater than zero, false otherwise.
    */
  def > (that: Rational) :Boolean = numer * that.denom > that.numer * denom

  /**
    * Checks if `this` is equal to OR greater than the provided `Rational`.
    * @return true iff this >= 0, false otherwise.
    */
  def >= (that:Rational) :Boolean = (this > that) || (this == that)

  /**
    * Determines if `this` is STRICTLY greater than the provided `BigInt`.
    * @param that An `BigInt` to compare `this` to.
    * @return true iff `this` is STRICTLY greater than `that`, false otherwise.
    */
  def > (that:BigInt) : Boolean = this > new Rational(that, 1)

  /**
    * Checks if `this` is bigger than OR equal to `that`.
    * @param that An `BigInt` to compare `this` to.
    * @return true iff `this` is  greater than OR equal to `that`, false otherwise.
    */
  def >= (that:BigInt) : Boolean = {
    val r = new Rational(that, 1)
    (this > r) || (this == r)       // Avoid calling constructor twice
  }

  /** Checks if `this` is STRICTLY smaller than the `Rational` provided.
    * @param that A `Rational` to compare this to.
    * @return true iff this < that, false otherwise.
    */
  def < (that:Rational) : Boolean = !(this >= that)

  /** Checks if `this` is  smaller than OR equal to the `Rational` provided.
    * @param that A `Rational` to compare this to.
    * @return true iff this <= that, false otherwise.
    */
  def <= (that:Rational) : Boolean = !(this > that)

  /**
    * Checks if `this` is STRICTLY smaller than the `BigInt` provided.
    * @param that An `BigInt` instance.
    * @return true iff this < that, false otherwise.
    */
  def <(that:BigInt): Boolean = {
    !(this >= that)
  }

  /**
    * Checks if `this` is smaller than OR equal to the `BigInt` provided.
    * @param that An `BigInt` instance.
    * @return true iff this <= that, false otherwise.
    */
  def <= (that:BigInt):Boolean = {
    !(this > that)
  }

  /**
    * Raises `this` to the provided power. The implementation is tail recursive
    * and through repeated squaring, such that the code executes in O(log_2n) iterations.
    * @param n the power to which we will raise the current `Rational` instance.
    * @return this raised to the nth power, as long as this and n are not both equal to 0.
    * @throws IllegalArgumentException if this = n = 0
    */
  def ^ (n:Int):Rational = {

    // Inner two-arg function
    def pow(base:BigInt, exp:Int):BigInt = {
      require(exp >= 0 && base >=0, "We need positive integer arguments for this method.")
      // Power computation with tail-recursive repeated squaring.
      @tailrec
      def pow(currExp:Int, maxExp:Int, currTerm:BigInt, prodAccum:BigInt): BigInt = {
        assert(currExp <= maxExp, "The current exponent should never surpass the original one.")
        if(currExp <= maxExp / 2)
          pow(2*currExp, maxExp, currTerm * currTerm, prodAccum)    // Next iteration on current term.
        else if (currExp == maxExp) currTerm  * prodAccum                   // Bottomed out.
        else  pow(1, maxExp - currExp, base, currTerm * prodAccum) // Compute residual product (rest of terms) using the same method.
      }

      if (base == 0 && exp == 0) throw new IllegalArgumentException("0^0 is an undefined form.")
      else if(base == 0 && exp > 0) 0
      else if(base > 0 && exp == 0) 1
      else if(exp == 1) base
      else if(base == 1) 1
      else pow(1, exp, base, 1)
    }

    if(n == 1) this
    else if(n == 0) ONE
    else if(n < 0) new Rational(pow(denom, -n), pow(numer, -n))
    else new Rational(pow(numer, n), pow(denom, n))
  }

}

/**
  * Companion object of class `Rational`.
  */
object Rational {

  /**
    * A `Rational` instance which represents 1/1=1.
    */
 val ONE = new Rational (1, 1)

  /**
    * * A `Rational` instance which represents  0/1=0.
    */
  val ZERO = new Rational (0, 1)

  /**
    ** A `Rational` instance which represents  (-1)/1.
    */
  val MINUSONEOVERONE = new Rational(-1, 1)

  /**
    ** A `Rational` instance which represents 1/(-1).
    */
  val ONEOVERMINUSONE = new Rational(1, -1)

  /**
    * A `Rational` instance which represents 0/0. Will throw an instance of
    * `IllegalArgumentException` *every* time it is evaluated by client, because if the
    * value of a lazy val cannot be computed, it has to be re-computed every time
    * (more info <a href="http://bit.ly/2IK21gi">here</a>). We make this value into a `lazy val` to
    * allow the `Rational` to be brought into life without throwing.
    */
   lazy val ZEROOVERZERO = new Rational(0, 0)
}