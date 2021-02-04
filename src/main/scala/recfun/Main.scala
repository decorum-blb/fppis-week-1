package recfun

import scala.annotation.tailrec

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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    *
    * Given a list of characters, determine if the list is "balanced" by ensuring that each open character in (, [, and {
    * are closed properly by a matching ), ], or } (respectively)
    */
  def balance(chars: List[Char]): Boolean = {
    def balancesChar(open: Char, close: Char): Boolean =
      close match {
        case ')' => open == '('
        case ']' => open == '['
        case '}' => open == '{'
        case _ => false
      }

    @tailrec
    def addOrRemoveChars(openChars: List[Char], remainingChars: List[Char]): List[Char] = {
      val OPEN_CHARACTERS = Set[Char]('(', '[', '{')
      val CLOSE_CHARACTERS = Set[Char](')', ']', '}')

      if (remainingChars.nonEmpty) {
        val char = remainingChars.head

        if (OPEN_CHARACTERS.contains(char)) {
          // if the current character is an open-character, then run addOrRemoveChars again
          // having added the current character to the head of the `openChars` List,
          // and remove the first element in `remainingChars`
          addOrRemoveChars(char :: openChars, remainingChars.tail)
        }
        else if (CLOSE_CHARACTERS.contains(char)) {
          openChars.headOption match {
              // if the current character, which is a close-character, balances out the most recent open-character
              // then run addOrRemoveChars again having removed the first element in both openChars and remainingChars
            case Some(openChar) if balancesChar(openChar, char) =>
              addOrRemoveChars(openChars.tail, remainingChars.tail)
              // if there are no elements in the `openChars` List, then our current character, which is a close-character,
              // has been reached without a preceding open-character.
              // This results in an un-balanced list of characters, which we will represent with the CLOSE_CHARACTERS Set as a List
              // and will force the boolean check of `isEmpty` at the end to return `false`, which is the intended result
            case _ => CLOSE_CHARACTERS.toList
          }
        }
        else {
          addOrRemoveChars(openChars, remainingChars.tail)
        }
      } else {
        // if `remainingChars` is empty, then return the List of `openChars`
        openChars
      }
    }

    addOrRemoveChars(List(), chars).isEmpty
  }

  /**
    * Exercise 3
    *
    * Write a recursive function that counts how many different ways you can make change for an amount,
    * given a list of coin denominations.
    *
    * For example, there are 3 ways to give change for 4 if you have coins
    * with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
    *
    * Do this exercise by implementing the countChange function in Main.scala.
    * This function takes an amount to change, and a list of unique denominations for the coins.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int], sum: Int): Int = {
      // if there are no coin denominations, then we have no way to determine ways of dividing our money
      if(coins.isEmpty) sum
      // if there is no money, then there's only one way to represent that value => 0
      else if (money == 0) sum + 1
      // in any case where the head element in coins has a greater value than money,
      // remove it from the coins list and try again
      else if (coins.head > money)
        loop(money, coins.tail, sum)
      // reduce money to 0, while accumulating a sum of "ways" by subtracting coins.head from money
      // in doing so, you'll  go through each iteration of each way to subtract from money
      // while accumulating a total
      else
        loop(money, coins.tail, sum + loop(money - coins.head, coins, 0))
    }

    loop(money, coins, 0)
  }
}
