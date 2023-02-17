package com.sambeth.algoexpert.easy

import scala.annotation.tailrec

object FindClosestValueInBst {

  private def findClosestValueInBstRecursion(tree: BinaryTree, target: Int): Int =

    @tailrec
    def findClosestNumber(tree: BinaryTree, closestSoFar: Int): Int =
      tree match
        case BinaryTree(value, right, _) if (value < target) && right.isEmpty =>
          closestSoFar
        case BinaryTree(value, right, _) if (value < target) && right.isDefined && (Math.abs(closestSoFar - target) > Math.abs(right.get.value - target)) =>
          findClosestNumber(right.get, right.get.value)
        case BinaryTree(value, right, _) if (value < target) && right.isDefined && (Math.abs(closestSoFar - target) < Math.abs(right.get.value - target)) =>
          findClosestNumber(right.get, closestSoFar)

        case BinaryTree(value, _, left) if (value > target) && left.isEmpty =>
          closestSoFar
        case BinaryTree(value, _, left) if (value > target) && left.isDefined && (Math.abs(closestSoFar - target) > Math.abs(left.get.value - target)) =>
          findClosestNumber(left.get, left.get.value)
        case BinaryTree(value, _, left) if (value > target) && left.isDefined && (Math.abs(closestSoFar - target) < Math.abs(left.get.value - target)) =>
          findClosestNumber(left.get, closestSoFar)

    findClosestNumber(tree, tree.value)


  @main def mainSix: Unit =
    val one = BinaryTree(1)
    val fourteen = BinaryTree(14)
    val fiveChild = BinaryTree(5)
    val twentyTwo = BinaryTree(22)
    val two = BinaryTree(2, left = Option(one))
    val fiveParent = BinaryTree(5, right = Option(fiveChild), left = Option(two))
    val thirteen = BinaryTree(13, right = Option(fourteen))
    val fifteen = BinaryTree(15, right = Option(twentyTwo), left = Option(thirteen))
    val root = BinaryTree(10, right = Option(fifteen), left = Option(fiveParent))

    println(findClosestValueInBstRecursion(root, 12))

}
