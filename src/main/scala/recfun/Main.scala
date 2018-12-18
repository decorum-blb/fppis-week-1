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
      open match {
        case '(' => close == ')'
        case '[' => close == ']'
        case '{' => close == '}'
        case _ => false
      }

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
    */
  def countChange(money: Int, coins: List[Int]): Int = 1
}
