package com.sambeth.algoexpert.easy

import scala.annotation.tailrec

object FindClosestValueInBst {

  class BST(var value: Int, var right: Option[BST] = None, var left: Option[BST] = None):

    override def toString: String = s"BST($value, right: $right, left: $left)"

  object BST {

    def apply(value: Int, right: Option[BST] = None, left: Option[BST] = None): BST =
      new BST(value, right, left)

    def unapply(bst: BST): Option[(Int, Option[BST], Option[BST])] =
      Some((bst.value, bst.right, bst.left))
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
    val tree = BST(10)
    val f_five = BST(5)
    val s_five = BST(5)
    val fifteen = BST(15)
    val two = BST(2)
    val thirteen = BST(13)
    val twenty_two = BST(22)
    val fourteen = BST(14)
    val one = BST(1)

    tree.left = Some(f_five)
    tree.right = Some(fifteen)
    f_five.left = Some(two)
    f_five.right = Some(s_five)
    fifteen.left = Some(thirteen)
    fifteen.right = Some(twenty_two)
    two.left = Some(one)
    thirteen.right = Some(fourteen)

    println(findClosestValueInBstRecursion(tree, 12))

}
