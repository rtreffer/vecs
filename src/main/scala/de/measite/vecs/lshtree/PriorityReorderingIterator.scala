package de.measite.vecs.lshtree

import java.util.Iterator
import java.util.TreeSet

import de.measite.vecs.data.KVector

import de.measite.vecs.xtree.Entry

class PriorityReorderingIterator(
  __center : KVector,
  __lookahead : Int,
  __iterator : Iterator[Entry]
) extends Iterator[Entry] {

  def remove() : Unit = {
    throw new UnsupportedOperationException
  }

  def score(e : Entry) : Double = {
    __center.distance(e.key)
  }

  class QueueEntry(
    _element : Entry,
    _score   : Double,
    _uuid    : Int
  ) extends Comparable[QueueEntry] {

    val element = _element
    val score   = _score
    val uuid    = _uuid

    override def compareTo(that : QueueEntry) : Int =
      if (this.score == that.score) {
        Math.signum(that.uuid - this.uuid)
      } else {
        Math.signum( this.score - that.score ).asInstanceOf[Int]
      }

    override def equals(obj : Any) : Boolean = {
      if (obj == null) { return false }
      if (!obj.isInstanceOf[AnyRef]) { return false }
      val that = obj.asInstanceOf[AnyRef]
      this eq that
    }

  }

  private var _next    = null.asInstanceOf[Entry]
  private var _cur     = null.asInstanceOf[Entry]
  private var _hasNext = true
  private var _uuid    = 0
  private var _queue   = new TreeSet[QueueEntry]

  def hasNext() : Boolean = {
    if (!_hasNext) {
      false
    } else {
      if (_next eq null) { _next = inext()  }
      if (_next eq null) { _hasNext = false }
      _hasNext
    }
  }

  def next() : Entry = {
    if (!_hasNext) {
      throw new IllegalStateException
    }
    if (_next eq null) {
      _cur = inext()
      _hasNext = (_cur ne null)
    } else {
      _cur  = _next
      _next = null.asInstanceOf[Entry]
    }
    _cur
  }

  private def inext() : Entry = {
    val lastScore = -1d
    while ((_queue.size < __lookahead) && (__iterator.hasNext)) {
      val e = __iterator.next().value.asInstanceOf[Entry]
      _queue.add(new QueueEntry(e, score(e), _uuid))
      _uuid += 1
    }
    _queue.pollFirst.element
  }

}

