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
  def pascal(c: Int, r: Int): Int = {
    def isBorderLine() =
      c == 0 || c == r

    if (isBorderLine()) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def updateCounter(character: Char, counter: Int): Int =
      if (character == '(') counter + 1 else if (character == ')') counter - 1 else counter

    def balanceIter(chars: List[Char], counter: Int): Boolean =
      if (counter < 0) false else if (chars.isEmpty) counter == 0 else balanceIter(chars.tail, updateCounter(chars.head, counter))

    balanceIter(chars.tail, updateCounter(chars.head, 0))
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(coins: List[Int]) : Int = 
       if(coins.length == 1) countChange(money - coins.head, coins) else countChange(money - coins.head, coins) + countChange(money, coins.tail)

    if(coins.isEmpty || money < 0) 0 else if(money == 0) 1 else countChangeIter(coins)
  }
}
