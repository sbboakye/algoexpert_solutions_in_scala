package com.sambeth.algoexpert.easy

import scala.annotation.tailrec

object FindClosestValueInBst {

  private def findClosestValueInBstRecursion(tree: BinaryTree, target: Int): Int =

    @tailrec
    def findClosestNumber(tree: BinaryTree, closestSoFar: Int): Int =
      tree match
        case BinaryTree(value, _, right) if (value < target) && right.isEmpty =>
          closestSoFar
        case BinaryTree(value, _, right) if (value < target) && right.isDefined && (Math.abs(closestSoFar - target) > Math.abs(right.get.value - target)) =>
          findClosestNumber(right.get, right.get.value)
        case BinaryTree(value, _, right) if (value < target) && right.isDefined && (Math.abs(closestSoFar - target) < Math.abs(right.get.value - target)) =>
          findClosestNumber(right.get, closestSoFar)

        case BinaryTree(value, left, _) if (value > target) && left.isEmpty =>
          closestSoFar
        case BinaryTree(value, left, _) if (value > target) && left.isDefined && (Math.abs(closestSoFar - target) > Math.abs(left.get.value - target)) =>
          findClosestNumber(left.get, left.get.value)
        case BinaryTree(value, left, _) if (value > target) && left.isDefined && (Math.abs(closestSoFar - target) < Math.abs(left.get.value - target)) =>
          findClosestNumber(left.get, closestSoFar)

    findClosestNumber(tree, tree.value)


  @main def mainSix: Unit =
    val root = BinaryTree(10)
      .add(5)
      .add(2)
      .add(1)
      .add(5)
      .add(15)
      .add(13)
      .add(14)
      .add(22)

    println(root)
    println(findClosestValueInBstRecursion(root, 12))

}
