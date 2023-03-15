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

    val root = BinaryTree(10)
      .add(15)
      .add(22)
      .add(13)
      .add(14)
      .add(5)
      .add(5)
      .add(2)
      .add(1)
    println(root)
    println(branchSumsOne(root))
    println(branchSumsTwo(root))
}
