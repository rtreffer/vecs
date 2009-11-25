package de.measite.vecs.xtree

import de.measite.vecs.data.KVector

class Entry(
  __key : KVector,
  __value: AnyRef
) {

  val key = __key
  var value = __value

}

