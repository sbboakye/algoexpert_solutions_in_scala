package com.sambeth.algoexpert.easy

case class BinaryTree(value: Int, left: Option[BinaryTree] = None, right: Option[BinaryTree] = None) {
  def add(aValue: Int): BinaryTree =
    insert(aValue).get

  private def insert(aValue: Int): Option[BinaryTree] =

    def insertLeft(aValue: Int): Option[BinaryTree] =
      left.flatMap(_.insert(aValue)) orElse Option(BinaryTree(aValue))

    def insertRight(aValue: Int): Option[BinaryTree] =
      right.flatMap(_.insert(aValue)) orElse Option(BinaryTree(aValue))

    aValue.compare(value) match {
      case 0 => Option(this)
      case -1 => Option(BinaryTree(value, insertLeft(aValue), right))
      case 1 => Option(BinaryTree(value, left, insertRight(aValue)))
    }
}
