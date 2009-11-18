package de.measite.vecs.xtree

import de.measite.vecs.data.KVector

class Entry(
  __key : KVector,
  __value: Array[Byte]
) {

  val key = __key
  var value = __value

}

