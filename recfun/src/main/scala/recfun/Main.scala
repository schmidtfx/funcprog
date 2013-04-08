package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(balance("((".toList))
    println(balance("".toList))

    println(countChange (4, List(1, 2)))
    println(countChange (15, List(1, 5, 10, 25)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0) 1 
    else if(c == 1 && r == 1) 1
    else if(c > r) 0
    else pascal(c-1, r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_intern(chars: List[Char], counter: Int): Boolean = {
      chars match {
        case Nil => counter == 0
        case head :: tail => 
          head match {
            case ')' =>
              if(counter == 0) false
              else balance_intern(tail, counter - 1)
            case '(' => balance_intern(tail, counter + 1)
            case _ => balance_intern(tail, counter)
        }
      }
    }

    if(chars.isEmpty) true
    else balance_intern(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val ways = Array.fill(money + 1)(0)
    ways(0) = 1
    coins.foreach (coin =>
      for (j<-coin to money)
        ways(j) =  ways(j) + ways(j - coin)
    )
    ways(money)
  }
}
