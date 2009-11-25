package de.measite.vecs

import de.measite.vecs.data.KVector
import de.measite.vecs.xtree.Tree
import de.measite.vecs.xtree.Entry

/**
 * 
 *
 */
object App extends Application {

  override def main(args: Array[String]) = {
    System.out.println("");
    val rnd = new Random(1337)
    val mul = 10000
    val vector = new Array[KVector](mul * 100)
    for (i <- 0 until vector.length) {
      val v = new Array[Double](7)
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
    var tree = new Tree(10)
    for (i <- 0 until vector.length) {
      tree.add(new Entry(vector(i), null))
      if ((i + 1) % mul == 0) {
        System.out.println("(2) " + System.currentTimeMillis + " " + ((i + 1) / mul) + "%");
      }
    }
    // Step 3: search
    for (i <- 0 until 10) {
      val v = vector(Math.abs(rnd.nextInt) % vector.length)
      var bestMatch : KVector = null
      var bestMatchScore = java.lang.Double.MAX_VALUE
      System.out.println("(3) Search (validation search)");
      var rtime = -System.currentTimeMillis
      var vi = 0
      while (vi < vector.length) {
        val vec = vector(vi)
        if (vec ne v) {
          val d = vec.distance(v)
          if (d < bestMatchScore) {
            bestMatchScore = d
            bestMatch = vec
          }
        }
        vi += 1
      }
      rtime += System.currentTimeMillis
      System.out.println("(3) Search tree");
      var iter = tree.search(v)
      var time = -System.currentTimeMillis
      var next = iter.next.key
      time += System.currentTimeMillis
      if (next ne v) {
        System.out.println("Searched: " + v);
        System.out.println("Found: " + next);
        System.out.println("Distance: " + (v distance next) + " : " + (next distance v));
        throw new IllegalStateException
      }
      System.out.println("(X) Need " + time + "ms instead of " + rtime + "ms");
      System.out.println("(3) Search tree: 1 passed");
      next = iter.next.key
      if (next ne bestMatch) {
        System.out.println("Searched: " + v);
        System.out.println("Expected: " + bestMatch);
        System.out.println("Found: " + next);
        System.out.println("Distance: " + (v distance next) + " : " + (next distance v));
        System.out.println("ExpectedDistance: " + bestMatchScore);
        throw new IllegalStateException
      }
      System.out.println("(3) Search tree: 2 passed");
      time = -System.currentTimeMillis
      iter = tree.search(v)
      var i = 0
      while (i < 100) {
        iter.next
        i += 1
      }
      System.out.println
      time += System.currentTimeMillis
      System.out.println("Time for 100 elements: " + time + "ms instead of " + rtime + "ms");
    }
    System.exit(0);

  }

}
