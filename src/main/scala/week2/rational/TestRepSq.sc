package week2.rational

import scala.annotation.tailrec

object TestRepSq extends App {
  def pow(b: Int, e: Int): Int = {
    if (e == 0) 1
    else if (e == 1) b
    else pow(1, e, b)

    // Power computation with tail-recursive repeated squaring!
    @tailrec
    def pow(currExp: Int, origExp: Int, currProd: Int): Int = {
      if (currExp < origExp) pow(2 * currExp, origExp, currProd * currProd) // Next iteration
      else if (currExp == origExp) currProd // Bottomed out
      else pow(1, origExp - currExp, currProd * currProd) // Compute residual product using the same method
    }
  }

  pow(2, 0)
  pow(2, 1)
  pow(2, 2)
  pow(2, 3)

  pow(3, 0)
  pow(3, 1)
  pow(3, 2)

  pow(4, 2)
  pow(2, 4)

  pow(5, 2)

  pow(2, 8)
  pow(2, 8)
}