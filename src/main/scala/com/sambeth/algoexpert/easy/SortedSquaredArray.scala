package com.sambeth.algoexpert.easy

import scala.annotation.tailrec

object SortedSquaredArray {

  private def sortedSquaredArrayUsingSorted(array: List[Int]): List[Int] =
    array.map(Math.pow(_, 2).toInt).sorted

  private def sortedSquaredArrayNotUsingSorted(array: List[Int]): List[Int] =
    val sortedArray: Array[Int] = Array.fill(array.length)(0)
    var startIndex = 0
    var endIndex = array.length - 1

    for
      idx <- array.indices.reverse
    do
      var smallValue = array(startIndex)
      var largeValue = array(endIndex)
      if Math.abs(largeValue) > Math.abs(smallValue) then
        sortedArray(idx) = largeValue * largeValue
        endIndex -= 1
      else
        sortedArray(idx) = smallValue * smallValue
        startIndex += 1

    sortedArray.toList

  private def sortedSquaredArrayRecursion(array: List[Int]): List[Int] =
    val sortedArray: List[Int] = Array.fill(array.length)(0).toList

    @tailrec
    def loopThroughArray(array: List[Int], sortedArray: List[Int], arrayIndex: Int, startIndex: Int, endIndex: Int): List[Int] =
      array match
        case Nil => sortedArray
        case head :: Nil =>
          sortedArray.updated(arrayIndex, head * head)
        case head :: tail if (Math.abs(head) < Math.abs(tail.last) || Math.abs(head) == Math.abs(tail.last)) =>
          loopThroughArray(array.dropRight(1), sortedArray.updated(arrayIndex, tail.last * tail.last), arrayIndex - 1, startIndex, endIndex - 1)
        case head :: tail if Math.abs(head) > Math.abs(tail.last) =>
          loopThroughArray(array.drop(1), sortedArray.updated(arrayIndex, head * head), arrayIndex - 1, startIndex + 1, endIndex)

    loopThroughArray(array, sortedArray, array.length - 1, 0, array.length - 1)


  @main def mainThree: Unit =
    println(sortedSquaredArrayUsingSorted(List(1,2,3,5,6,8,9)))
    println(sortedSquaredArrayNotUsingSorted(List(-2, -1)))
    println(sortedSquaredArrayNotUsingSorted(List(-5, -4, -3, -2, -1)))
    println(sortedSquaredArrayRecursion(List(1, 2, 3, 5, 6, 8, 9)))
    println(sortedSquaredArrayRecursion(List(-2, -1)))
    println(sortedSquaredArrayRecursion(List(-5, -4, -3, -2, -1)))
    println(sortedSquaredArrayRecursion(List(-5, -4, -3, -2, -1, 0, 1, 2, 3)))
    println(sortedSquaredArrayRecursion(List(-2, -1, 0, 1, 2, 3, 4, 5)))
    println(sortedSquaredArrayNotUsingSorted(List(-2, -1, 0, 1, 2, 3, 4, 5)))

}
