package de.measite.vecs.xtree

import de.measite.vecs.data.KVector

class SplitHistory(
  __dim : Int,
  __value : Double,
  __parent : SplitHistory,
  __left : SplitHistory,
  __right : SplitHistory,
  __leftNode : Node,
  __rightNode : Node
) {

  val dim = __dim
  val value = __value
  var parent = __parent
  var left = __left
  var right = __right
  var leftNode = __leftNode
  var rightNode = __rightNode

  if (left ne null) {
    left.parent = this
  }
  if (right ne null) {
    right.parent = this
  }

  def this(
    __dim : Int,
    __value : Double,
    __left : SplitHistory,
    __right : SplitHistory
  ) {
    this(__dim, __value, null, __left, __right, null, null)
  }

  def this(
    __dim : Int,
    __value : Double,
    __left : Node,
    __right : Node
  ) {
    this(__dim, __value, null, null, null, __left, __right)
  }

  def toArray(array : Array[Node]) : Int = {
    _toArray(array, 0)
  }

  private def _toArray(array : Array[Node], pos : Int) : Int = {
    var npos = pos
    if (left eq null) {
      array(npos) = leftNode
      npos += 1
    } else {
      npos += left._toArray(array, npos)
    }
    if (right eq null) {
      array(npos) = rightNode
      npos += 1
    } else {
      npos += right._toArray(array, npos)
    }
    npos - pos
  }

  def count : Int = {
    if (left eq null) { 1 } else { left.count }
  } + {
    if (right eq null) { 1 } else { right.count }
  }

  def getNode(vector : KVector) : Node = {
    if (vector.dimension(dim) <= value) {
      if (leftNode ne null) { leftNode } else { left.getNode(vector) }
    } else {
      if (rightNode ne null) { rightNode } else { right.getNode(vector) }
    }
  }

  def replace(node : Node, split : SplitHistory) : Boolean = {
    if (leftNode eq node) {
      leftNode = null
      left = split
      left.parent = this
      true
    } else
    if (rightNode eq node) {
      rightNode = null
      right = split
      right.parent = this
      true
    } else {
      if ((left ne null) && left.replace(node, split)) { true } else
      if ((right ne null) && right.replace(node, split)) { true } else
      { false }
    }
  }

}

