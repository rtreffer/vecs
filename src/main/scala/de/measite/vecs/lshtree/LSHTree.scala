package de.measite.vecs.lshtree

import java.util.Iterator

import de.measite.vecs.data.KVector

import de.measite.vecs.xtree.Tree
import de.measite.vecs.xtree.Entry

/**
 * A LSH with an X-Tree backend and Geometric Average distance function.
 * A geometric average will be 0 as long as a single hashvalue matches,
 * thus revealing the same results as a LSH with a hashtable per dimension.
 * However an X-Tree is multidimensional and thus will reveal results with a
 * certain fuzziness.
 */
class LSHTree(__dimension : Int, __granuality : Int, __width : Int) {

  val hash = Array(
    new LSH(__dimension, __granuality),
    new LSH(__dimension, __granuality),
    new LSH(__dimension, __granuality),
    new LSH(__dimension, __granuality),
    new LSH(__dimension, __granuality),
    new LSH(__dimension, __granuality),
    new LSH(__dimension, __granuality)
  )

  val tree = new Tree(__width)

  def hash(v : KVector) : KVector = {
    val vkey = new Array[Double](hash.length)
    var i = 0
    while (i < hash.length) {
      vkey(i) = hash(i).hash(v)
      i += 1
    }
    new KVector(vkey)
  }

  def add(e : Entry) : Unit = {
   tree.add(new Entry(hash(e.key), e))
  }

  def search(key : KVector) : Iterator[Entry] = {
    new PriorityReorderingIterator(
      key,
      2000,
      tree.search(hash(key))
    )
  }

}

