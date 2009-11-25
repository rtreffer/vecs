package de.measite.vecs.xtree

import java.util.BitSet
import java.util.Arrays
import java.lang.Double.isNaN

import de.measite.vecs.data.{KVector, RRectangle}

class Node(
  __node  : Array[Node],
  __nodes : Int,
  __entry : Array[Entry],
  __entries : Int,
  __parent : Node,
  __tree : Tree
) {

  def this(
    __node : Array[Node],
    __nodes : Int,
    __parent : Node,
    __tree : Tree
  ) = {
    this(
      __node, __nodes,
      null, 0,
      __parent, __tree
    )
  }

  def this(
    __entry : Array[Entry],
    __entries : Int,
    __parent : Node,
    __tree : Tree
  ) = {
    this(
      null, 0,
      __entry, __entries,
      __parent, __tree
    )
  }

  var node = __node
  private var nodes = __nodes
  var entry = __entry
  private var entries = __entries
  private var parent = __parent
  private var tree = __tree
  var bounds = RRectangle.NULL

  {
    var i = 0
    while (i < entries) {
      bounds += entry(i).key
      i += 1
    }
    i = 0
    while (i < nodes) {
      bounds += node(i).bounds
      node(i).parent = this
      i += 1
    }
  }

  private var splitHistory : SplitHistory = _

  private def splitLeafDimension : Int = {
    val maxDimension = bounds.low.dimension.length
    var i = 0
    var splitDimension = -1
    var splitSpan = -1d
     while (i < maxDimension) {
      val l = bounds.low.dimension(i)
      val h = bounds.high.dimension(i)
      if ((!isNaN(l)) && (!isNaN(h)) && (l != h) && (h - l > splitSpan)) {
        splitSpan = h - l
        splitDimension = i
      }
      i += 1
    }
    splitDimension
  }

  private def splitLeaf : Unit = {

    val splitDimension = splitLeafDimension
    if (splitDimension == -1) {
      // TODO grow limit
      // TODO sth goes horribly wrong with leaf growing
      val grown = new Array[Entry](entry.length + tree.width)
      System.arraycopy(entry, 0, grown, 0, entry.length)
      entry = grown
      return
    } else {
      Arrays.sort(entry, new EntryComparator(splitDimension))

      val splitPosition = entry.length / 2

      val splitValue = (
        entry(splitPosition).key.dimension(splitDimension) +
        entry(splitPosition + 1).key.dimension(splitDimension)
      ) / 2d

      val leftEntries = splitPosition
      val leftEntry = new Array[Entry](Math.max(leftEntries + 1, tree.width))
      val rightEntries = entry.length - splitPosition
      val rightEntry = new Array[Entry](Math.max(rightEntries + 1, tree.width))

      System.arraycopy(entry, 0, leftEntry, 0, leftEntries)
      System.arraycopy(entry, splitPosition, rightEntry, 0, rightEntries)

      val left = new Node(leftEntry, leftEntries, null, tree)
      val right = new Node(rightEntry, rightEntries, null, tree)

      val rootChild = new Array[Node](tree.width)
      rootChild(0) = left
      rootChild(1) = right

      val split = new SplitHistory(splitDimension, splitValue, left, right)

      if (parent eq null) {
        tree.root = new Node(rootChild, 2, null, tree)
        tree.root.splitHistory = split
      } else {
        parent.replaceNode(this, split)
      }
    }
  }

  def splitNode() : Unit = {
    if ((splitHistory.left eq null)
    ||  (splitHistory.right eq null)
    ||  (splitHistory.left.count * 3 < tree.width)
    ||  (splitHistory.right.count * 3 < tree.width)) {
      // grow :-(
      // TODO upper grow limit
      val grown = new Array[Node](node.length + tree.width)
      System.arraycopy(node, 0, grown, 0, node.length)
      node = grown
    } else {
      // split :-)
      val leftSize = ((splitHistory.left.count / tree.width) + 1) * tree.width
      val rightSize = ((splitHistory.right.count / tree.width) + 1) * tree.width
      val leftNodes = new Array[Node](leftSize)
      val leftLen = splitHistory.left.toArray(leftNodes)
      val left = new Node(leftNodes, leftLen, null, tree)
      val rightNodes = new Array[Node](rightSize)
      val rightLen = splitHistory.right.toArray(rightNodes)
      val right = new Node(rightNodes, rightLen, null, tree)

      left.splitHistory = splitHistory.left
      right.splitHistory = splitHistory.right

      val split = new SplitHistory(
        splitHistory.dim, splitHistory.value,
        left, right
      )

      if (parent eq null) {
        // root split
        val rootNode = new Array[Node](tree.width)
        rootNode(0) = left
        rootNode(1) = right
        tree.root = new Node(rootNode, 2, null, tree)
        tree.root.splitHistory = split
      } else {
        // inner split
        parent.replaceNode(this, split)
      }
    }
  }

  def replaceNode(oldNode : Node, split : SplitHistory) : Unit = {
    if (!splitHistory.replace(oldNode, split)) {
      throw new IllegalStateException
    }
    var i = 0
    while (i < nodes) {
      if (node(i) eq oldNode) {
        node(i) = split.leftNode
        node(nodes) = split.rightNode
        nodes += 1
        split.leftNode.parent = this
        split.rightNode.parent = this
        if (nodes == node.length) { splitNode }
        return
      }
      i += 1
    }
    throw new IllegalStateException
  }

  def add(e : Entry) : Unit = {
    bounds += e.key
    if (__entry ne null) {
      // leaf level node
      entry(entries) = e
      entries += 1
      if (entries == entry.length) { splitLeaf }
      e
    } else {
      // inner node
      splitHistory.getNode(e.key).add(e)
    }
  }

}

