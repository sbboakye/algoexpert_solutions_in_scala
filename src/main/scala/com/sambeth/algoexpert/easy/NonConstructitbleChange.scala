package com.sambeth.algoexpert.easy

object NonConstructitbleChange {

  private def nonConstructitbleChange(coins: List[Int]): Int =

    var currentChange = 0

    for coin <- coins.sorted
      do
        if coin > currentChange + 1 then currentChange + 1
        else
          currentChange = currentChange + coin

    currentChange + 1

  @main def mainFive: Unit =
    println(nonConstructitbleChange(List(5, 7, 1, 1, 2, 3, 22)))

}
