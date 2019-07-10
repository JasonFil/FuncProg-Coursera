package week2.rational

object LazyEval extends App {

  lazy val x = {
    println("Evaluating x")
    scala.math.pow(3, 10)
  }
  println(x) ; println(x)
}

