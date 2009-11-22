package de.measite.vecs.data

import java.lang.Double.isNaN

object RRectangle {
  val NULL = new RRectangle()
}

/**
 * A RRectangle is the Rectangle spawned by a minimum and maximum vector.
 *
 * This class is collections safe. It implements equals, compareTo and
 * hashCode
 */
class RRectangle(
  val __low: KVector,
  val __high: KVector
) extends Comparable[RRectangle] {

  val low  = __low
  val high = __high

  /**
   * Create a new zero-dimensional rectangle.
   */
  def this() {
    this(
      new KVector(new Array[Double](0)),
      new KVector(new Array[Double](0))
    )
  }

  /**
   * Check if this area contains a vector. Return false if the vector contains
   * dimensions unknown by this rectangle.
   */
  def contains(that: KVector) : Boolean = {
    ((low <= that) && (that <= high))
  }

  /**
   * Check if this rectangle contains another rectangle. False if the other
   * rectangle contains dimensions unknown to this dimension.
   */
  def contains(that: RRectangle) : Boolean = {
    contains(that.low) &&  contains(that.high)
  }

  /**
   * Compute the distance of this rectangle and a given vector.
   */
  def distance(that : KVector) : Double = {
    var result = 0d
    var i = 0
    val len = that.dimension.length
    val lenlow = low.dimension.length
    val lenhigh = high.dimension.length
    val limit = Math.min(len, Math.max(lenlow, lenhigh))
    while (i < limit) {
      val v = that.dimension(i)
      val l = low.dimension(i)
      val h = high.dimension(i)
      if ((!isNaN(v)) && (!isNaN(l))) {
        if (v < l) {
          result += (v - l) * (v - l)
        } else
        if (v > h) {
          result += (v - h) * (v - h)
        }
     }
     i += 1
    }
    result
  }

  def llk(that : KVector, k : Double) : Double = {
    var result = 0d
    val limit = Math.min(that.dimension.length, low.dimension.length)
    var i = 0
    while (i < limit) {
      val v = that.dimension(i)
      val l = low.dimension(i)
      val h = high.dimension(i)
      if ((!isNaN(v)) && (!isNaN(l))) {
        val d =
          if (v < l) { Math.abs(v - l).asInstanceOf[Double] } else
          if (v > h) { Math.abs(v - h).asInstanceOf[Double] } else
          { 0d }
        result += Math.pow(d, k)
      }
      i += 1
    }
    result
  }

  def geometricDistance(that : KVector) : Double = {
    var result = 0d
    val limit = Math.min(that.dimension.length, low.dimension.length)
    var i = 0
    while (i < limit) {
      val v = that.dimension(i)
      val l = low.dimension(i)
      val h = high.dimension(i)
      if ((!isNaN(v)) && (!isNaN(l))) {
        result *= {
          if (v < l) { Math.abs(v - l) } else
          if (v > h) { Math.abs(v - h) } else
          { 0d }
        }
        if (result == 0d) { return 0d }
      }
      i += 1
    }
    result
  }

  /**
   * Add another area, return a rectangle containing both areas
   */
  def +(that: RRectangle) : RRectangle = {
    val l = this.low .min(that.low )
    val h = this.high.max(that.high)
    if ((l eq that.low) && (h eq that.high)) {
      return that
    }
    if ((l eq low) && (h eq high)) {
      return this
    }
    new RRectangle(l, h)
  }

  /**
   * Add another vector, return a rectangle containing the new vector
   */
  def +(that: KVector) : RRectangle = {
    val l = this.low .min(that)
    val h = this.high.max(that)
    if ((l eq low) && (h eq high)) {
      return this
    }
    new RRectangle(l, h)
  }

  /**
   * Compute the intersection with another area
   */
  def intersection(that: RRectangle) : RRectangle = {
    val maxlow  = this.low .max(that.low,  false)
    val minhigh = this.high.min(that.high, false)
    // If intersection == this
    if ((this.low eq maxlow) && (this.high eq minhigh)) { return this }
    // If intersection == that
    if ((that.low eq maxlow) && (that.high eq minhigh)) { return that }
    // If intersection == that && this.low == that.low
    if ((this.low eq maxlow) && (that.high eq minhigh)) {
      if (this.low  == that.low ) { return that }
    }
    // It intersection == that && this.high == that.high
    if ((that.low eq maxlow) && (this.high eq minhigh)) {
      if (this.high == that.high) { return that }
    }
    // Unique intersection
    val len =
      Math.min(
        maxlow.dimension.length,
        minhigh.dimension.length
      )
    var isNull = true
    val low  = new Array[Double](len)
    val high = new Array[Double](len)
    var i = 0;
    while (i < len) {
      val l = maxlow.dimension(i)
      val h = minhigh.dimension(i)
      if (!isNaN(l) && !isNaN(h)) {
        if (l <= h) {
          low(i)  = l
          high(i) = h
        } else {
          // no intersectiont in this dimension == no intersection at all
          return RRectangle.NULL
        }
        isNull = false
      } else {
        low(i)  = Double.NaN
        high(i) = Double.NaN
      }
      i += 1
    }
    if (isNull) {
      RRectangle.NULL
    } else {
      new RRectangle(new KVector(low), new KVector(high))
    }
  }

  def split(
    dim   : Int,
    value : Double
  ) : (RRectangle, RRectangle) = {
    val thislen = this.low.dimension.length

    if (dim >= thislen) {
      if (value >= 0d) { return (this, null) }
      if (value <  0d) { return (null, this) }
      return (this, null)
    }

    val l = this.low.dimension(dim)
    if (isNaN(l))   {
      if (value >= 0d) { return (this, null) }
      if (value <  0d) { return (null, this) }
      return (null, this)
    }
    if (l >= value)    { return (this, null) }

    val h = this.high.dimension(dim)

    if (h <= value)    { return (null, this) }
    if (l == h)        { return (null, this) }

    val splitLow  = new Array[Double](thislen)
    val splitHigh = new Array[Double](thislen)
    System.arraycopy(low.dimension,  0, splitLow,  0, thislen)
    System.arraycopy(high.dimension, 0, splitHigh, 0, thislen)
    splitLow(dim)  = value
    splitHigh(dim) = value
    (new RRectangle(low, new KVector(splitHigh)), new RRectangle(new KVector(splitLow), high))
  }

  def union(that : RRectangle) : Array[RRectangle] = {
    // TODO
    var lcr = true
    var rcl = true
    val thislen = this.low.dimension.length
    val thatlen = that.low.dimension.length
    val len = Math.max(thislen, thatlen)
    var i = 0
    while (i < len && (lcr || rcl)) {
      val l1 = if (i < thislen) { this. low.dimension(i) } else { Double.NaN }
      val h1 = if (i < thislen) { this.high.dimension(i) } else { Double.NaN }
      val l2 = if (i < thatlen) { that. low.dimension(i) } else { Double.NaN }
      val h2 = if (i < thatlen) { that.high.dimension(i) } else { Double.NaN }
      if ( l1 < l2 ) {
      
      } else
      if ( l1 > l2 ) {

      }
    }
    null
  }

  var _diagonal : KVector = null

  def diagonal : KVector = {
    if (_diagonal == null) {
      _diagonal = low.to(high)
    }
    _diagonal
  }

  var _center : KVector = null

  def center : KVector = {
    if (_center == null) { 
      _center = low avg high
    }
    _center
  }

  var _area1p : Double = Double.NaN
  var _area   : Double = Double.NaN

  /**
   * Compute the area of this rectangle such that
   * <ul>
   *   <li>result  = 1</li>
   *   <li>result *= (high(i) - low(i) + 1) // for every i</li>
   *   <li>result  = result - 1</li>
   * </ul>
   */
  def area1p : Double = {
    if (isNaN(_area1p)) {
      _area1p = 1d
      var i = 0
      val lowlen = low.dimension.length
      val highlen = high.dimension.length
      val limit = Math.min(lowlen, highlen)
      while (i < limit) {
        val l = low.dimension(i)
        val h = high.dimension(i)
        if ((!isNaN(l)) && (!isNaN(h))) {
          _area1p *= (h - l + 1d)
        }
        i += 1
      }
      _area1p -= 1d
    }
    _area1p
  }

  def area : Double = {
    if (isNaN(_area)) {
      val lowlen  =  low.dimension.length
      val highlen = high.dimension.length
      val limit   = Math.min(lowlen, highlen)
      var first   = true
      var i = 0
      while (i < limit) {
        val l =  low.dimension(i)
        val r = high.dimension(i)
        if ((!isNaN(l)) && (!isNaN(r))) {
          val d = (r - l) * (r - l)
          if (d > 0d) {
            if (first) {
              _area  = d
              first  = false
            } else {
              _area *= d
            }
          }
        }
        i += 1
      }
      if (first) { _area = 0d }
    }
    _area
  }

  override def toString() : String = {
    val sb = new StringBuilder()
    sb.append("RRectangle<")
    low.apply(
      high,
      (p,l,h) => { sb.append("(" + p + "," + l + "," + h + ")") },
      (p,l)   => {                                              },
      (p,h)   => {                                              },
      (p)     => {                                              }
    )
    sb.append(">")
    sb.toString
  }

  override def equals(obj : Any) : Boolean = {
    if ((obj == null)||(!obj.isInstanceOf[RRectangle])) {
      return false
    } else {
      val that = obj.asInstanceOf[RRectangle]
      if (this eq that) {
        true
      } else {
        that.low.equals(this.low) && that.high.equals(this.high)
      }
    }
  }

  override def hashCode() : Int = {
    low.hashCode * 31 + high.hashCode
  }

  override def compareTo(that: RRectangle) : Int = {
    val v = this.low.compareTo(that.low)
    if (v != 0) {
      v
    } else {
      this.high.compareTo(that.high)
    }
  }

}

