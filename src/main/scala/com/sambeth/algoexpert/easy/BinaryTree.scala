package com.sambeth.algoexpert.easy

case class BinaryTree(value: Int, right: Option[BinaryTree], left: Option[BinaryTree])

object BinaryTree {
  def apply(value: Int, right: Option[BinaryTree] = None, left: Option[BinaryTree] = None): BinaryTree =
    new BinaryTree(value, right, left)
}
