package com.sambeth.algoexpert.easy

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object BranchSums {

  private def branchSumsOne(root: BinaryTree): ListBuffer[Int] =

    def getBranchSums(root: Option[BinaryTree], sums: ListBuffer[Int] = ListBuffer.empty, runningSum: Int): ListBuffer[Int] =

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


  private def branchSumsTwo(root: BinaryTree): ListBuffer[Int] =

    def getBranchSums(root: Option[BinaryTree], sums: ListBuffer[Int] = ListBuffer.empty, runningSum: Int): ListBuffer[Int] =

      root match
        case None => sums
        case Some(root) =>
          val newRunningSum = root.value

          root match
            case BinaryTree(_, right: Some[BinaryTree], left: Some[BinaryTree]) =>
              getBranchSums(left, sums, runningSum + newRunningSum)
              getBranchSums(right, sums, runningSum + newRunningSum)
            case BinaryTree(_, _: None.type, left: Some[BinaryTree]) => getBranchSums(left, sums, runningSum + newRunningSum)
            case BinaryTree(_, right: Some[BinaryTree], _: None.type) => getBranchSums(right, sums, runningSum + newRunningSum)
            case BinaryTree(_, _: None.type, _: None.type) => sums += runningSum + newRunningSum

    getBranchSums(root = Option(root), runningSum = 0)


  @main def mainSeven: Unit =

    val one = BinaryTree(1)
    val fourteen = BinaryTree(14)
    val fiveChild = BinaryTree(5)
    val twentyTwo = BinaryTree(22)
    val two = BinaryTree(2, left = Option(one))
    val fiveParent = BinaryTree(5, right = Option(fiveChild), left = Option(two))
    val thirteen = BinaryTree(13, right = Option(fourteen))
    val fifteen = BinaryTree(15, right = Option(twentyTwo), left = Option(thirteen))
    val root = BinaryTree(10, right = Option(fifteen), left = Option(fiveParent))

    println(root)
    println(branchSumsOne(root))
    println(branchSumsTwo(root))

}
