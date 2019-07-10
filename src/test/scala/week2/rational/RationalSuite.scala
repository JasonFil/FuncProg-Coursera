package week2.rational
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RationalSuite extends FunSuite {
  import week2.rational.Rational._

  private def frac(a:BigInt, b:BigInt) : Rational = new Rational(a, b)

  test("Bad Rationals"){
    intercept[IllegalArgumentException]{
      frac(0, 0)
    }

    intercept[IllegalArgumentException]{
      ZEROOVERZERO
    }
  }

  test("Basic construction and constants"){

    assert(frac(2, 4) === frac(1, 2))
    assert(frac(1, 2).eval === 0.5)
    assert(ZERO.eval === 0)
    assert(ONEOVERMINUSONE === frac(1, -1))
    assert(MINUSONEOVERONE === frac(-1, 1))
    assert(ONEOVERMINUSONE === MINUSONEOVERONE)
  }

  test("Unary operators"){

  }

  test("Binary operators"){
    assert(frac(2, 3) + frac(3, 4) === frac(17, 12))
    assert(frac(2, 3) - frac(3, 4) === frac(-1, 12))
    assert(frac(-2, 3) === frac(2, -3))
    assert(frac(-2, 3) + frac(2, 3) === frac(0, 9))

    assert(frac(1, 3) * 3 === frac(1, 3) + frac(1, 3) + frac(1, 3))

    assert(frac(1, 5) + frac(2, 7) + frac(3, 4) - frac(1, 8) === frac(311, 280))

    assert(frac(0, 1) * 1 === frac(0, 1))
    assert(frac(2,3) - frac(-2, 3) === frac(4, 3))
  }

  test("Comparisons with zero"){
    assert(MINUSONEOVERONE < 0)
    assert(ONEOVERMINUSONE < 0)
    assert(frac(-3, 4) < 0)
    assert(frac(-3, 4) <= 0)
    assert(frac(3, 4) > 0)
    assert(frac(3, 4) >= 0)
    assert(ZERO >= 0)
  }

  test("Comparisons between Rational instances"){
    assert(ZERO >= ZERO)
    assert(ONEOVERMINUSONE >= MINUSONEOVERONE)
    assert(ZERO > ONEOVERMINUSONE)
    assert(ZERO >= ONEOVERMINUSONE)
    assert(frac(1, 9) < frac(9, 1))
    assert(frac(9, 1) > frac(1, 9))
    assert(frac(1, 3) > frac(1, 4))
    assert((frac(1, 3) ^ 2 )> (frac(1, 4) ^ 2))
  }

  test("Exponentiation"){

    assert(( frac(2, 3) ^ 1 )  === frac(2, 3))

    assert(( frac(2, 3) ^ -1 )  === frac(3, 2))

    assert(( frac(2, 3) ^ 2 )  === frac(4, 9))
    assert(( frac(2, 3) ^ -2 )  === frac(9, 4))

    assert(( frac(2, 3) ^ 10) === frac(1024, 59049))
  }


}
