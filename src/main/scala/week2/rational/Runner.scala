package week2.rational

object Runner extends App {

  private def get(a:Int, b:Int) = {
    new Rational(a, b)
    // a / b
  }



  val a = get(5, 6)
  val b = get(2, 3)
  val c = get(1, 2)

  println(a * b + c)
}
