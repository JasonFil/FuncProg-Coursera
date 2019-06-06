package week1

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Assignment.countChange
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3) // 1 + 1 + 1 + 1, 1 + 1 + 2, 2 + 2
  }

  // The tests I wrote in comments of Main.scala
  test("jason1"){
    assert(countChange(1, List(1)) === 1)
  }

  test("jason2"){
    assert(countChange(2, List(1)) === 1)
  }

  test("jason3"){
    assert(countChange(6, List(2, 3)) === 2)
  }


  test("jason4"){
    assert(countChange(11, List(11, 4, 9, 6)) === 1)
  }

  test("jason5"){
    assert(countChange(11, List(13, 4, 9, 6)) === 0)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

}
