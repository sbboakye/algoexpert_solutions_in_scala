package com.sambeth.algoexpert.easy

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object BranchSums {

  case class BST(value: Int, right: Option[BST], left: Option[BST])

  object BST {
    def apply(value: Int, right: Option[BST] = None, left: Option[BST] = None): BST =
      new BST(value, right, left)
  }

  private def branchSumsOne(root: BST): ListBuffer[Int] =

    def getBranchSums(root: Option[BST], sums: ListBuffer[Int] = ListBuffer.empty, runningSum: Int): ListBuffer[Int] =

      root match
        case None => sums
        case Some(root) =>
          val newRunningSum = root.value

          if root.left.isEmpty && root.right.isEmpty then
            sums += runningSum + newRunningSum
          else
            getBranchSums(root.left, sums, runningSum + newRunningSum)
            getBranchSums(root.right, sums, runningSum + newRunningSum)

    getBranchSums(root=Option(root), ListBuffer.empty, runningSum = 0)


  private def branchSumsTwo(root: BST): ListBuffer[Int] =

    def getBranchSums(root: Option[BST], sums: ListBuffer[Int] = ListBuffer.empty, runningSum: Int): ListBuffer[Int] =

      root match
        case None => sums
        case Some(root) =>
          val newRunningSum = root.value

          root match
            case BST(_, right: Some[BST], left: Some[BST]) =>
              getBranchSums(left, sums, runningSum + newRunningSum)
              getBranchSums(right, sums, runningSum + newRunningSum)
            case BST(_, _: None.type, left: Some[BST]) => getBranchSums(left, sums, runningSum + newRunningSum)
            case BST(_, right: Some[BST], _: None.type) => getBranchSums(right, sums, runningSum + newRunningSum)
            case BST(_, _: None.type, _: None.type) => sums += runningSum + newRunningSum

    getBranchSums(root = Option(root), runningSum = 0)


  @main def mainSeven: Unit =

    val one = BST(1)
    val fourteen = BST(14)
    val fiveChild = BST(5)
    val twentyTwo = BST(22)
    val two = BST(2, left = Option(one))
    val fiveParent = BST(5, right = Option(fiveChild), left = Option(two))
    val thirteen = BST(13, right = Option(fourteen))
    val fifteen = BST(15, right = Option(twentyTwo), left = Option(thirteen))
    val root = BST(10, right = Option(fifteen), left = Option(fiveParent))

    println(root)
    println(branchSumsOne(root))
    println(branchSumsTwo(root))

}
