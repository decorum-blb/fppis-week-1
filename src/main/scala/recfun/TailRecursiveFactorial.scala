package recfun

import scala.annotation.tailrec

object TailRecursiveFactorial {
  def main(args: Array[String]): Unit = {
    println(factorial(4))
  }

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if(n == 0) acc
      else n * factorial(n - 1)

    loop(1, n)
  }
}
