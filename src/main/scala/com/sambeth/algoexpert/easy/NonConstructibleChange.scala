package com.sambeth.algoexpert.easy

import scala.annotation.tailrec

object NonConstructibleChange {

  private def nonConstructibleChange(coins: List[Int]): Int =

    var currentChange = 0

    for coin <- coins.sorted
      do
        if coin > currentChange + 1 then currentChange + 1
        else
          currentChange = currentChange + coin

    currentChange + 1

  private def nonConstructibleChangeRecursion(coins: List[Int]): Int =
    @tailrec
    def findNonConstructibleChange(coins: List[Int], currentChange: Int): Int =
      coins match
        case Nil => currentChange + 1
        case List(head, _*) if head > currentChange + 1 => currentChange + 1
        case head :: tail if head <= currentChange + 1 => findNonConstructibleChange(tail, currentChange + head)

    findNonConstructibleChange(coins.sorted, 0)

  @main def mainFive: Unit =
    println(nonConstructibleChange(List(5, 7, 1, 1, 2, 3, 22)))
    println(nonConstructibleChangeRecursion(List(5, 7, 1, 1, 2, 3, 22)))

}
