package com.sambeth.algoexpert.easy

import scala.annotation.tailrec

object IsValidSequence {

  private def isValidSequenceWhileLoop(array: List[Int], sequence: List[Int]): Boolean =
    if sequence.length == 1 then
      if array.contains(sequence.length) then true
      else false

    else if sequence.length > array.length then false
    else
      var arrIndex = 0
      var seqIndex = 0
      while arrIndex < array.length & seqIndex < sequence.length do
        if array(arrIndex) == sequence(seqIndex) then seqIndex += 1
        else arrIndex += 1

      seqIndex == sequence.length


  private def isValidSequenceForLoop(array: List[Int], sequence: List[Int]): Boolean =
    if sequence.length == 1 then
      if array.contains(sequence.length) then true
      else false

    else if sequence.length > array.length then false
    else
      var currIndex = 0
      var countdown = sequence.length
      for
        num <- array
        if currIndex < sequence.length
        if sequence(currIndex) == num
      do
          currIndex += 1
          countdown -= 1

      countdown == 0

  private def isValidSequenceFP(array: List[Int], sequence: List[Int]): Boolean =
    @tailrec
    def loopThroughArray(seq: List[Int], index: Int, countdown: Int): Boolean =
      seq match
        case head :: tail if head == array(index) => loopThroughArray(tail, index + 1, countdown - 1)
        case head :: tail if head != array(index) => loopThroughArray(tail, index, countdown)
        case Nil => countdown == 0

    loopThroughArray(sequence, 0, sequence.length)

  @main def mainTwo: Unit =
    println(isValidSequenceForLoop(List(5, 1, 22, 25, 6, -1, 8, 10), List(22, 25, 6))) // true
    println(isValidSequenceWhileLoop(List(5, 1, 22, 25, 6, -1, 8, 10), List(22, 25, 6))) // true
    println(isValidSequenceWhileLoop(List(5, 1, 22, 25, 6, -1, 8, 10), List(5, 3, 12, 21, 22, 25, 6, 9, 10))) // false
    println(isValidSequenceFP(List(1, 22, 25, 6, -1, 8, 10), List(1, 2, 22))) // false
    println(isValidSequenceFP(List(1, 1, 1, 1, 1), List(1, 1, 1))) // true
}
