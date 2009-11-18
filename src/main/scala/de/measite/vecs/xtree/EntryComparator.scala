package de.measite.vecs.xtree

class EntryComparator(
  dimension : Int
) extends java.util.Comparator[Entry] {

  def compare(
    left : Entry,
    right : Entry
  ) : Int = {
    val l = left.key.dimension(dimension)
    val r = right.key.dimension(dimension)
    if (l < r) { return -1 }
    if (l > r) { return  1 }
    0
  }

}

