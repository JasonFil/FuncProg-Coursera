package week1

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  import Assignment.balance

  // Positive tests
  test("balance: empty string is balanced"){
    assert(balance("".toList))
  }

  test("balance: left and right paren balanced"){
    assert(balance("()".toList))
  }

  test("balance: left and right paren with stuff in between balanced"){
    assert(balance("(stuff)".toList))
  }

  test("balance: Embedded parentheses balanced."){
    assert(balance("(())".toList))
  }

  test("balance: Embedded parenthesized substrings balanced."){
    assert(balance("()(word(more(stuff))red(cliff))".toList))
  }

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  // Negative tests

  test("One open paren"){
    assert(!balance("(".toList))
  }

  test("One closed paren"){
    assert(!balance(")".toList))
  }

  test("Flipped opening and closing parens"){
    assert(!balance(")(".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }

  test("Complex test"){
    assert(!balance("((()())()))".toList))
  }

}
