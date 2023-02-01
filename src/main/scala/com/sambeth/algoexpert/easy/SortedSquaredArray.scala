package com.sambeth.algoexpert.easy

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

  private def sortedSquaredArrayFP(array: List[Int]): List[Int] =
    ???


  @main def mainThree: Unit =
    println(sortedSquaredArrayUsingSorted(List(1,2,3,5,6,8,9)))
    println(sortedSquaredArrayNotUsingSorted(List(-2, -1)))
    println(sortedSquaredArrayNotUsingSorted(List(-5, -4, -3, -2, -1)))

}
