package de.measite.vecs.xtree

import java.util.Iterator
import java.util.TreeSet

import de.measite.vecs.data.KVector

class NNIterator(
  __center : KVector,
  __start : Node
) extends Iterator[Entry] {

  class QueueEntry[E](
    _element : E,
    _score   : Double,
    _uuid    : Int
  ) extends Comparable[QueueEntry[AnyRef]] {

    val element = _element
    val score   = _score
    val uuid    = _uuid

    override def compareTo(that : QueueEntry[AnyRef]) : Int =
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

  def terminal ( element : Node ) = element.entry
  def expand ( element : Node ) = element.node

  def scoreElement ( element : Node ) =
    element.bounds.distance(__center)

  def scoreTerminal ( element : Entry ) =
    element.key.distance(__center)

  def remove() : Unit = {
    throw new UnsupportedOperationException
  }

  val innerQueue = new TreeSet[QueueEntry[Node]]
  val terminals  = new TreeSet[QueueEntry[Entry]]
  { innerQueue.add(new QueueEntry[Node](__start, scoreElement(__start), -1)) }

  private var _next    = null.asInstanceOf[Entry]
  private var _cur     = null.asInstanceOf[Entry]
  private var _hasNext = true
  private var _uuid    = 0

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
    while (innerQueue.size > 0) {
      val e = innerQueue.pollFirst
      val tHead = terminals.pollFirst
      if (tHead ne null){
        if (tHead.compareTo(e.asInstanceOf[QueueEntry[AnyRef]]) < 0) {
          innerQueue.add(e)
          return tHead.element
        } else {
          terminals.add(tHead)
        }
      }
      val t = terminal(e.element)
      if (t == null) {
        val array = expand(e.element)
        if (array != null) {
          var i = 0
          while (i < array.length) {
            val child = array(i)
            if (child ne null) {
              innerQueue.add(new QueueEntry[Node](child, scoreElement(child), _uuid))
              _uuid += 1
            }
            i += 1
          }
        }
      } else {
        var i = 0
        while (i < t.length) {
          val terminal = t(i)
          if (terminal ne null) {
            terminals.add(new QueueEntry[Entry](terminal, scoreTerminal(terminal), _uuid))
            _uuid += 1
          }
          i += 1
        }
      }
    }
    val t = terminals.pollFirst
    if (t ne null) {
      t.element
    } else {
      null.asInstanceOf[Entry]
    }
  }

}

