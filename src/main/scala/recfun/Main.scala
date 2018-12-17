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

    var isBalanced = true

    def addOrRemoveChars(openChars: List[Char], remainingChars: List[Char]): List[Char] = {
      val OPEN_CHARACTERS = Set[Char]('(', '[', '{')
      val CLOSE_CHARACTERS = Set[Char](')', ']', '}')

      if (remainingChars.nonEmpty) {
        val char = remainingChars.head

        if (OPEN_CHARACTERS.contains(char)) {
          addOrRemoveChars(char :: openChars, remainingChars.tail)
        }
        else if (CLOSE_CHARACTERS.contains(char)) {
          openChars.headOption match {
            case Some(openChar) if balancesChar(openChar, char) =>
              addOrRemoveChars(openChars.tail, remainingChars.tail)
            case _ =>
              isBalanced = false
              addOrRemoveChars(openChars, remainingChars.tail)
          }
        }
        else {
          addOrRemoveChars(openChars, remainingChars.tail)
        }
      } else {
        openChars
      }
    }

    addOrRemoveChars(List(), chars).isEmpty && isBalanced
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = 1
}
