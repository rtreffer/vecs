package de.measite.vecs.xtree

import java.util.Iterator

import de.measite.vecs.data.KVector

class Tree(
  __width : Int
) {

  var root : Node = new Node(new Array[Entry](__width), 0, null, this)

  val width = __width

  def add(key : KVector, value : Array[Byte]) : Unit = {
    root.add(key, value);
  }

  def search(key : KVector) : Iterator[Entry] = new NNIterator(key, root)

  def geometricSearch(key : KVector) : Iterator[Entry] =
    new GeomIterator(key, root)

  def search(key : KVector, k : Double) : Iterator[Entry] =
    new LLKIterator(key, k, root)

}

