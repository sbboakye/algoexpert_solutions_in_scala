package com.sambeth.algoexpert.easy

import scala.annotation.tailrec

object FindClosestValueInBst {

  case class BST(value: Int, right: Option[BST], left: Option[BST])

  object BST {
    def apply(value: Int, right: Option[BST] = None, left: Option[BST] = None): BST =
      new BST(value, right, left)
  }

  private def findClosestValueInBstRecursion(tree: BST, target: Int): Int =

    @tailrec
    def findClosestNumber(tree: BST, closestSoFar: Int): Int =
      tree match
        case BST(value, right, _) if (value < target) && right.isEmpty =>
          closestSoFar
        case BST(value, right, _) if (value < target) && right.isDefined && (Math.abs(closestSoFar - target) > Math.abs(right.get.value - target)) =>
          findClosestNumber(right.get, right.get.value)
        case BST(value, right, _) if (value < target) && right.isDefined && (Math.abs(closestSoFar - target) < Math.abs(right.get.value - target)) =>
          findClosestNumber(right.get, closestSoFar)

        case BST(value, _, left) if (value > target) && left.isEmpty =>
          closestSoFar
        case BST(value, _, left) if (value > target) && left.isDefined && (Math.abs(closestSoFar - target) > Math.abs(left.get.value - target)) =>
          findClosestNumber(left.get, left.get.value)
        case BST(value, _, left) if (value > target) && left.isDefined && (Math.abs(closestSoFar - target) < Math.abs(left.get.value - target)) =>
          findClosestNumber(left.get, closestSoFar)

    findClosestNumber(tree, tree.value)


  @main def mainSix: Unit =
    val one = BST(1)
    val fourteen = BST(14)
    val fiveChild = BST(5)
    val twentyTwo = BST(22)
    val two = BST(2, left = Option(one))
    val fiveParent = BST(5, right = Option(fiveChild), left = Option(two))
    val thirteen = BST(13, right = Option(fourteen))
    val fifteen = BST(15, right = Option(twentyTwo), left = Option(thirteen))
    val root = BST(10, right = Option(fifteen), left = Option(fiveParent))

    println(findClosestValueInBstRecursion(root, 12))

}
