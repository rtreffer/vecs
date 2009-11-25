package de.measite.vecs.lshtree

import java.util.Random

import de.measite.vecs.data.KVector

/**
 * A locality sensitive hash, based on p-stable distribution.
 */
class LSH(__dimension : Int, __buckets : Int) {

  val buckets = __buckets
  val a = {
    val vec = new Array[Double](__dimension)
    val rnd = new Random
    var i = 0
    while(i < __dimension) {
      vec(i) = rnd.nextGaussian
      i += 1
    }
    new KVector(vec)
  }

  def hash(vector : KVector) : Double = {
    val d = Math.abs(vector * a)
    Math.round(d * buckets) % buckets
  }

}

