package week1

object Assignment {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if( (c == 0)  || (c == r) ) 1 else pascal(c - 1, r-1) + pascal(c, r - 1)

  /**
   * Exercise 2
    *
    * The input data type doesn't allow me to make jumps on the string that it represents. For this reason,
    * I will give an implementation with an accumulator. TODO: see if you can come up with a tail-recursive implementation
    * that uses either a List[Char] or, barring progress with that, a String.
   */
    def balance(chars: List[Char]): Boolean = {

      def balance(chars:List[Char], accum:String): Boolean ={
        if(chars.isEmpty) accum.isEmpty
        else if(chars.head == '(') balance(chars.tail, '(' + accum) // Pushing up front to make it possible to take the tail of accum. Using `String` for `accum` for simplicity.
        else if(chars.head  == ')'  && accum.isEmpty) false
        else if(chars.head == ')') balance(chars.tail, accum.substring(1))
        else balance(chars.tail, accum)
      }

      balance(chars, "")
    }
  
  /**
   * Exercise 3
    *
    * @param money A non-negative integer representing the  &quot;available&quot; money at the current stack frame.
    * @param coins A - not necessarily sorted - list of strictly positive `Int`s which denote the available bill denominations.
    * @return The number of combinations of `coins` which sum to `money`.
    *
   */

   /*
    * TODO: (1) If you sort the list first, you might be able make this faster.
    *       (2) See if you can also make it faster by using mod / div.
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty) 0                     // Exhausted denominations, backtrack to other possible solutions
      else if (money < 0)  0                  // Overblew available money, have to backtrack.
      else if(money == 0)  1                // If money is exhausted, we paid for it in full. Mark one way.
      else if(money > 0)
        countChange(money - coins.head, coins) + countChange(money, coins.tail) // Try first with current denom, and then with others.
      else 0
    }
  }


/* "--->" should be interpreted as "should be".


-----------------------------------------------------------------------------------
(1, [1]) ---> 1

                countChange(0,[1])         +              countChange(1, [])

                        1                  +                   0

                                           1

-----------------------------------------------------------------------------------
(2, [1]) ---> 1

                  cC(1, [1])               +                 cC(2, [])

       cC(0, [1])     +       cC(1, [])                         0

            1         +         0

                      1                    +                      0

                                           1

------------------------------------------------------------------------------------------
(6, [2, 3]) ---> 2


                      cC(4, [2, 3])                                      +                            cC(6, [3])

       cC(2, [2, 3])         +                  cC(4, [3])

cC(0, [2, 3]) + cC(2, [3])           cC(1, [3])       +          cC(4, [])             .... Should also produce 1....

      1  + cC(-1, [3]) + cC(2, [])  cC(-2, [3]) + cC(1, [])            0                                    .

              0       +     0         0         +     0                                                     .

                       0                        0                                                           .

              1                                                                                             .
                            1                                           +                                   1


                                                                         2

------------------------------------------------------------------------------------------

(11, [11, 4, 9, 6]) ---> 1


------------------------------------------------------------------------------------------

(11, [13, 4, 9, 6]) ---> 0

 */