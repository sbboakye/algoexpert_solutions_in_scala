package com.sambeth.algoexpert.easy

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable

object TwoNumberSum {

  private def twoNumberSumOne(array: List[Int], targetSum: Int): List[Int] =
    val hashMap: mutable.Map[Int, Boolean] = mutable.Map.empty

    for
      num <- array
    do
      val numToFind: Int = targetSum - num
      if hashMap.contains(numToFind) then return List(numToFind, num)
      else hashMap.put(num, true)

    List()

  private def twoNumberSumTwo(array: List[Int], targetSum: Int): List[Int] =
    val sortedList = array.sorted
    var left_pointer = 0
    var right_pointer = sortedList.length - 1

    while(left_pointer < right_pointer) {
      if sortedList(left_pointer) + sortedList(right_pointer) == targetSum then return List(sortedList(left_pointer), sortedList(right_pointer))
      else if sortedList(left_pointer) + sortedList(right_pointer) < targetSum then left_pointer += 1
      else if sortedList(left_pointer) + sortedList(right_pointer) > targetSum then right_pointer -= 1
    }

    List()


  private def twoNumberSumFP(array: List[Int], targetSum: Int): List[Int] =
    @tailrec
    def findPair(hashMap: immutable.Map[Int, Boolean], curIndex: Int): List[Int] =
      val curNum = array(curIndex)
      val maybeNum = targetSum - curNum
      hashMap.get(maybeNum) match {
        case Some(value) => List(curNum, maybeNum)
        case None => findPair(hashMap + (curNum -> true), curIndex + 1)
      }

    findPair(immutable.Map(), 0)


  @main def main: Unit =
    val output = twoNumberSumFP(List(3, 5, -4, 8, 11, 1, -1, 6), 10)
    println(output)
}
