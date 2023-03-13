package com.sambeth.algoexpert.easy

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MinimumWaitTime {

  private def minimumWaitTime(queries: List[Int]): Int =
    val sortedQueries = queries.sorted
    var waitTime = 0
    val waitTImes = ListBuffer[Int]()

    for
      i <- 0 until sortedQueries.length - 1
    do
      waitTime = waitTime + sortedQueries(i)
      waitTImes += waitTime

    waitTImes.sum

  private def minimumWaitTimeRecursion(queries: List[Int]): Int =

    @tailrec
    def getMinimumWaitTime(queries: List[Int], waitTime: Int, waitTimes: ListBuffer[Int]): Int =
      queries.sorted match
        case head :: tail => getMinimumWaitTime(tail, waitTime + head, waitTimes += waitTime)
        case Nil => waitTimes.sum

    getMinimumWaitTime(queries, waitTime = 0, waitTimes = ListBuffer[Int]())

  private def minimumWaitTimeRecursionAnother(queries: List[Int]): Int =

    @tailrec
    def getMinimumWaitTime(queries: List[Int], totalWaitTime: Int, queriesLeft: Int): Int =
      queries.sorted match
        case head :: tail => getMinimumWaitTime(tail, totalWaitTime + (head * queriesLeft), queriesLeft - 1)
        case Nil => totalWaitTime

    getMinimumWaitTime(queries, totalWaitTime = 0, queriesLeft = queries.length - 1)


  @main def mainTen: Unit =
    println(minimumWaitTime(List(2,1,6,2,3)))
    println(minimumWaitTimeRecursion(List(2,1,6,2,3)))
    println(minimumWaitTimeRecursionAnother(List(2,1,6,2,3)))

}
