package com.sambeth.algoexpert.easy

import scala.collection.mutable.ListBuffer

object DepthFirstSearch {

  class Node(name: String, val children: ListBuffer[Node] = ListBuffer.empty):

    def addChild(name: String): Unit =
      children += Node(name)

    def depthFirstSearch(array: ListBuffer[String]): ListBuffer[String] =
      array += name

      for (child <- children) {
        child.depthFirstSearch(array)
      }

      array

    override def toString: String = s"Node($name, $children)"

  @main def mainNine: Unit =
    val f = Node("F")
    f.addChild("I")
    f.addChild("J")
    val g = Node("G")
    g.addChild("K")
    val b = Node("B")
    b.addChild("E")
    b.children.append(f)
    val d = Node("D")
    d.children.append(g)
    d.addChild("H")
    val node = Node("A")
    node.children.append(b)
    node.addChild("C")
    node.children.append(d)

    println(node)
    println(node.depthFirstSearch(ListBuffer[String]()))
}
