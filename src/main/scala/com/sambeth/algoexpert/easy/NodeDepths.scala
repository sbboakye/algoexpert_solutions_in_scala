package com.sambeth.algoexpert.easy

import scala.collection.mutable.ListBuffer

object NodeDepths {

  private def nodeDepthsOne(root: BinaryTree): Int =

    def getNodeDepths(root: Option[BinaryTree], sums: ListBuffer[Int] = ListBuffer.empty, depth: Int): ListBuffer[Int] =

      root match
        case None => sums
        case Some(root) =>

          root match
            case BinaryTree(_, left: Some[BinaryTree], right: Some[BinaryTree]) =>
              sums += depth
              getNodeDepths(left, sums, depth + 1)
              getNodeDepths(right, sums, depth + 1)
            case BinaryTree(_, _: None.type, right: Some[BinaryTree]) => getNodeDepths(right, sums, depth + 1)
            case BinaryTree(_, left: Some[BinaryTree], _: None.type) => getNodeDepths(left, sums, depth + 1)
            case BinaryTree(_, _: None.type, _: None.type) => sums += depth + 1

    getNodeDepths(root = Option(root), depth = 0).sum


  private def nodeDepthsTwo(root: BinaryTree): Int =

    def getNodeDepths(root: Option[BinaryTree], depth: Int): Int =

      root match
        case None => 0
        case Some(root) => depth + getNodeDepths(root.left, depth + 1) + getNodeDepths(root.right, depth + 1)

    getNodeDepths(root = Option(root), depth = 0)

  @main def mainEight: Unit =
    val root = BinaryTree(10)
      .add(15)
      .add(22)
      .add(13)
      .add(14)
      .add(5)
      .add(2)
      .add(1)
      .add(5)
    println(root)
    println(nodeDepthsOne(root))
    println(nodeDepthsTwo(root))

}
