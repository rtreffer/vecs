package de.measite.vecs.xtree

import java.util.Iterator
import java.util.TreeSet

import de.measite.vecs.data.KVector

class GeomIterator(
  __center : KVector,
  __start : Node
) extends NNIterator(__center, __start) {

  override def scoreElement ( element : Node ) =
    element.bounds.geometricDistance(__center)

  override def scoreTerminal ( element : Entry ) =
    element.key.geometricDistance(__center, true)

}

