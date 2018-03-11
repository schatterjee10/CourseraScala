package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Balance method")
    checkIfBalanced("(if (zero? x) max (/ 1 x))")
    checkIfBalanced("I told him (that it’s not (yet) done). (But he wasn’t listening)")
    checkIfBalanced("())(")
    checkIfBalanced(":-)")

    println("Count Change")
    val l1 = (1 to 2).toList
    val numWays = countChange(4, l1)
    println(s"$l1 can be used in $numWays ways")
  }

  // for testing only
  def checkIfBalanced(str: String) = {
    val retval = balance(str.toList)
    if (retval) println(s"$str String is balanced") else println(s"$str String not balanced")
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =  {
      def isGoodClosing(chars: List[Char], n: Int): Boolean = {
        if (chars.isEmpty) {
          if (n == 0) true else false
        } else {
          val nextChar = chars.head
          val num =  
            nextChar match {
              case ')' => n - 1
              case '(' => n + 1
              case _   => n 
            }
          if (num < 0) false else isGoodClosing(chars.tail, num) 
        }
      }

      isGoodClosing(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty) 0
      else if (money < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
