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
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if ((c == 0) || (c == r)) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def openLoop(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty)
        if (open == 0) true else false
      else if ((chars.head == '('))
        if (chars.tail.isEmpty) false else openLoop(chars.tail, open + 1) // finishing with '(' -> false
      else if (chars.head == ')') openLoop(chars.tail, open - 1)
      else openLoop(chars.tail, open)
    }
    if (chars.isEmpty) true
    else if (chars.head == '(') openLoop(chars.tail, 1)
    else if (chars.head == ')') false
    else balance(chars.tail)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if ((coins.isEmpty) || (money < 0)) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
