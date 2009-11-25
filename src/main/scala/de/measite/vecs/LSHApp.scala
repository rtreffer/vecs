package de.measite.vecs

import java.util.TreeSet
import java.util.Comparator

import de.measite.vecs.data.KVector
import de.measite.vecs.lshtree.LSHTree
import de.measite.vecs.xtree.Entry

/**
 * 
 *
 */
object LSHApp extends Application {

  case class QueueEntry(
    distance : Double,
    pos : KVector,
    index : Int
  ) extends Comparable[QueueEntry] {
    def compareTo(that : QueueEntry) : Int = {
      val result = Math.signum(distance - that.distance)
      if (result == 1d || result == -1d) {
        result.asInstanceOf[Int]
      } else {
        Math.signum(index - that.index).asInstanceOf[Int]
      }
    }
  }

  def stat(center : KVector, result : Array[KVector]) : Unit = {
    val delta = new Array[Double](result.length)
    for (i <- 0 until result.length) {
      delta(i) = center distance result(i)
    }
    val avg = delta.reduceLeft((a,b) => {a + b}) / result.length
    val min = delta.reduceLeft((a,b) => Math.min(a,b))
    val max = delta.reduceLeft((a,b) => Math.max(a,b))
    System.out.println("MIN/AVG/MAX " + min + " / " + avg + " / " + max)
  }

  override def main(args: Array[String]) = {
    System.out.println("")
    System.out.println("LSH Test")
    val rnd = new Random(1337)
    val mul = 5000
    val vector = new Array[KVector](mul * 100)
    val k = 10
    for (i <- 0 until vector.length) {
      val v = new Array[Double](300)
      for (j <- 0 until v.length) {
        v(j) = rnd.nextDouble
      }
      vector(i) = new KVector(v)
      if ((i + 1) % mul == 0) {
        System.out.println("(1) " + ((i + 1) / mul) + "%");
      }
    }
    // Step 2: build a tree
    System.out.println("(2) tree");
    var tree = new LSHTree(300, 30, 10)
    for (i <- 0 until vector.length) {
      tree.add(new Entry(vector(i), java.lang.Integer.toString(i)))
      if ((i + 1) % mul == 0) {
        System.out.println("(2) " + System.currentTimeMillis + " " + ((i + 1) / mul) + "%");
      }
    }

    // Step 3: search
    for (i <- 0 until 30) {
      val v = vector(Math.abs(rnd.nextInt) % vector.length)
      var bestMatch : KVector = null
      var bestMatchScore = java.lang.Double.MAX_VALUE
      System.out.println("(3) Search (validation search)")
      val queue = new TreeSet[QueueEntry]
      var rtime = -System.currentTimeMillis
      var vi = 0
      while (vi < vector.length) {
        val vec = vector(vi)
        queue.add(new QueueEntry(vec distance v, vec, vi))
        if (queue.size > 100) {
          queue.pollLast
        }
        vi += 1
      }
      rtime += System.currentTimeMillis
      vi = 0
      val refArray = new Array[KVector](100)
      while (vi < 100) {
        refArray(vi) = queue.pollFirst.pos
//        System.out.println("(" + vi + ") " + tree.hash(refArray(vi)))
        vi += 1
      }
      var time = -System.currentTimeMillis
      var iter = tree.search(v)
      var i = 0
      while (i < 100) {
        iter.next
        i += 1
      }
      time += System.currentTimeMillis
      System.out.println("Time for 100 elements: " + time + "ms instead of " + rtime + "ms")
      stat(v, refArray)
      val resArray = new Array[KVector](100)
      iter = tree.search(v)
      for (i <- 0 until 100) {
        resArray(i) = iter.next.key
      }
      stat(v, resArray)
      // Random array as final reference
      for (i <- 0 until 100) {
        resArray(i) = vector(Math.abs(rnd.nextInt) % vector.length)
      }
      stat(vector(Math.abs(rnd.nextInt) % vector.length), resArray)
    }
    System.exit(0);

  }

}
