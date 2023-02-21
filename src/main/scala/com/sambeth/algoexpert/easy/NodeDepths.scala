package com.sambeth.algoexpert.easy

import scala.collection.mutable.ListBuffer

object NodeDepths {

  private def nodeDepthsOne(root: BinaryTree): Int =

    def getNodeDepths(root: Option[BinaryTree], sums: ListBuffer[Int] = ListBuffer.empty, depth: Int): ListBuffer[Int] =

      root match
        case None => sums
        case Some(root) =>

          root match
            case BinaryTree(_, right: Some[BinaryTree], left: Some[BinaryTree]) =>
              sums += depth
              getNodeDepths(left, sums, depth + 1)
              getNodeDepths(right, sums, depth + 1)
            case BinaryTree(_, _: None.type, left: Some[BinaryTree]) => getNodeDepths(left, sums, depth + 1)
            case BinaryTree(_, right: Some[BinaryTree], _: None.type) => getNodeDepths(right, sums, depth + 1)
            case BinaryTree(_, _: None.type, _: None.type) => sums += depth + 1

    getNodeDepths(root = Option(root), depth = 0).sum


  private def nodeDepthsTwo(root: BinaryTree): Int =

    def getNodeDepths(root: Option[BinaryTree], depth: Int): Int =

      root match
        case None => 0
        case Some(root) => depth + getNodeDepths(root.left, depth + 1) + getNodeDepths(root.right, depth + 1)

    getNodeDepths(root = Option(root), depth = 0)

  @main def mainEight: Unit =
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
    println(nodeDepthsOne(root))
    println(nodeDepthsTwo(root))

}
