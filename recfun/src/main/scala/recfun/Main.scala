package recfun

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
    if (c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def inner(chars: List[Char], current: Int): Boolean = {
      if (current < 0) false
      else if (chars.isEmpty) {
        current == 0
      } else if (chars.head == '(') inner(chars.tail, current + 1)
      else if (chars.head == ')') inner(chars.tail, current - 1)
      else inner(chars.tail, current)

    }

    inner(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def inner(money: Int, coins: List[Int], count: Int): Int = {
      if (money == 0 || coins.isEmpty) 0
      else if (money < coins.head) 0
      else if (money == coins.head) 1
      else inner(money - coins.head, coins, 0) + inner(money, coins.tail, 0)
    }
    inner(money, coins.sorted, 0)
  }
}
