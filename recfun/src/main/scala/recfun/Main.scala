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
    def pascal(m: Int, n: Int): Int = {
      def factorial(x: Int): Int = {
        @tailrec
        def inner(acc: Int, x: Int): Int = {
          if (x==0) acc else inner(acc*x, x-1)
        }
        inner(1, x)
      }
      factorial(n) / (factorial(m) * factorial(n - m))
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def inner (balance: Int, chars: List[Char]): Int = {
        if (balance<0) balance
        else chars match {
          case Nil => balance
          case '(' :: rest => inner(balance+1, rest)
          case ')' :: rest => inner(balance-1, rest)
          case head :: rest => inner(balance, rest)
        }
      }
      inner(0, chars) == 0
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money==0) 1
      else if (money>0 && !coins.isEmpty) {
        countChange(money-coins.head, coins) + countChange(money, coins.tail)
      }
      else 0
    }
  }
