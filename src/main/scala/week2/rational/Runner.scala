package week2.rational

object Runner extends App {

  private def fraction(a:Int, b:Int) = {
    require(b!= 0, "Cannot create a fraction with denominator zero.")
      new Rational(a, b)
     a.toDouble / b.toDouble
  }

  val a = fraction(5, 6)
  val b = fraction(2, 3)
  val c = fraction(1, 2)
  val d = fraction(10, 11)
  val e = fraction(25, 15) // Rational will reduce to 5/3
  val f = fraction(23, 13)
  val g = fraction(230, 17)
  println(a * b + c*d - e * (f + g))

}
