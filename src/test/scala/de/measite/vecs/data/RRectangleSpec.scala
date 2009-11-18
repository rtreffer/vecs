package de.measite.vecs.data

import org.specs._

import java.lang.Double.NaN

object RRectangleSpec extends Specification {

  val vec__ = new KVector(new Array[Double](0))
  val vec0_ = new KVector(Array(0d))
  val vec1_ = new KVector(Array(1d))
  val vec2_ = new KVector(Array(2d))
  val vecN_ = new KVector(Array(NaN))
  val vec00 = new KVector(Array(0d,0d))
  val vec01 = new KVector(Array(0d,1d))
  val vec02 = new KVector(Array(0d,2d))
  val vec0N = new KVector(Array(0d,NaN))
  val vec10 = new KVector(Array(1d,0d))
  val vec11 = new KVector(Array(1d,1d))
  val vec12 = new KVector(Array(1d,2d))
  val vec1N = new KVector(Array(1d,NaN))
  val vec20 = new KVector(Array(2d,0d))
  val vec21 = new KVector(Array(2d,1d))
  val vec22 = new KVector(Array(2d,2d))
  val vec2N = new KVector(Array(2d,NaN))
  val vecN0 = new KVector(Array(NaN,0d))
  val vecN1 = new KVector(Array(NaN,1d))
  val vecN2 = new KVector(Array(NaN,2d))
  val vecNN = new KVector(Array(NaN,NaN))
  val vec33 = new KVector(Array(3d,3d))

  val rrect0011 = new RRectangle(vec00, vec11)
  val rrect1021 = new RRectangle(vec10, vec21)
  val rrect0112 = new RRectangle(vec01, vec12)
  val rrect1111 = new RRectangle(vec11, vec11)
  val rrect1122 = new RRectangle(vec11, vec22)
  val rrect0022 = new RRectangle(vec00, vec22)
  val rrect1133 = new RRectangle(vec11, vec33)

  "Rectangle add" in {
    "simple add" in {
      (rrect0011 + rrect1122) must_== rrect0022
    }
    "instance check" in {
      ((rrect0022 + rrect0011) eq rrect0022) must_== true
      ((rrect0011 + rrect0022) eq rrect0022) must_== true
    }
  }

  "Rectangle intersection" in {
    "single point intersection" in {
      (rrect0011 intersection rrect1122) must_== rrect1111
      (rrect1122 intersection rrect0011) must_== rrect1111
    }
    "self intersection" in {
      ((rrect0011 intersection rrect0022) eq rrect0011) must_== true
      ((rrect0022 intersection rrect0011) eq rrect0011) must_== true
    }
    "area intersection" in {
      (rrect0022 intersection rrect1133) must_== rrect1122
      (rrect1133 intersection rrect0022) must_== rrect1122
    }
  }

  "Rectangle area1p" in {
     "simple area" in {
       rrect1122.area1p must_== 3d
     }
  }

  "Rectangle distance" in {
    "simple distance" in {
      (rrect0011 distance vec12) must_== 1d
      (rrect0011 distance vec21) must_== 1d
    }
    "diag distance" in {
      (rrect0011 distance vec22) must_== 2d
      (rrect0011 distance vec33) must_== 8d
    }
  }

}

