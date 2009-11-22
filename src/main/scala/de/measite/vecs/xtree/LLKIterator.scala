package de.measite.vecs.xtree

import java.util.Iterator
import java.util.TreeSet

import de.measite.vecs.data.KVector

class LLKIterator(
  __center : KVector,
  __k : Double,
  __start : Node
) extends NNIterator(__center, __start) {

  override def scoreElement ( element : Node ) =
    element.bounds.llk(__center, __k)

  override def scoreTerminal ( element : Entry ) =
    element.key.llk(__center, __k)

}

