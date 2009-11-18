package de.measite.vecs.data

import org.specs._

import java.lang.Double.NaN

object KVectorSpec extends Specification {

  val vec121a = new KVector(Array(1d,2d,1d))
  val vec123a = new KVector(Array(1d,2d,3d))
  val vec123b = new KVector(Array(1d,2d,3d))
  val vec124a = new KVector(Array(1d,2d,4d))
  val vec358a = new KVector(Array(3d,5d,8d))
  val vec12_a = new KVector(Array(1d,2d))
  val vec12Na = new KVector(Array(1d,2d,NaN))
  val vec1N1a = new KVector(Array(1d,NaN,1d))
  val vec1N3a = new KVector(Array(1d,NaN,3d))
  val vec1N3b = new KVector(Array(1d,NaN,3d))
  val vec321a = new KVector(Array(3d,2d,1d))
  val vec3N3a = new KVector(Array(3d,NaN,3d))
  val vec323a = new KVector(Array(3d,2d,3d))
  val vec32_a = new KVector(Array(3d,2d))
  val vec___a = new KVector(new Array[Double](0))

  "Vector create" should {
    "empty vector" in {
      vec___a.dimension.length must_== 0
    }
    "3 elements" in {
      vec123a.dimension(0) must_== 1d
      vec123a.dimension(1) must_== 2d
      vec123a.dimension(2) must_== 3d
    }
  }

  "Vector equals" should {
    "self" in {
      vec123a.equals(vec123a) must_== true
    }
    "array reference mathc" in {
      val array = Array(1d,2d,3d)
      val v1 = new KVector(array)
      val v2 = new KVector(array)
      v1.equals(v2) must_== true
      v2.equals(v1) must_== true
    }
    "content match" in {
      vec123a.equals(vec123b) must_== true
      vec123b.equals(vec123a) must_== true
    }
    "content mismatch" in {
      vec123a.equals(vec124a) must_== false
      vec124a.equals(vec123a) must_== false
    }
    "content NaN mismatch" in {
      vec12Na.equals(vec123a) must_== false
      vec123a.equals(vec12Na) must_== false
    }
    "Non-Vector" in {
      vec123a.equals(new Object()) must_== false
    }
    "NaN" in {
      vec1N3a.equals(vec1N3a) must_== true
      vec1N3b.equals(vec1N3b) must_== true
    }
    "NaN/length" in {
      vec12_a.equals(vec12Na) must_== true
      vec12Na.equals(vec12_a) must_== true
    }
  }

  "Vector add" should {
    "dense add" in {
      val vector =
        new KVector(Array(1d, 2d)) + new KVector(Array(3d, 4d))
      vector must_== new KVector(Array(4d,6d))
    }
    "variable length add" in {
      val vector =
        new KVector(Array(1d, 2d)) + new KVector(Array(3d, 4d, 5d))
      vector must_== new KVector(Array(4d,6d,5d))
    }
  }

  "Vector distance" should {
    "dense distance" in {
      vec123a.distance(vec358a) must_== 38
      vec358a.distance(vec123a) must_== 38
    }
    "sparse distance" in {
      vec12Na.distance(vec358a) must_== 13
      vec358a.distance(vec12Na) must_== 13
    }
    "variable lenth distance" in {
      vec12_a.distance(vec358a) must_== 13
      vec358a.distance(vec12_a) must_== 13
    }
  }

  "Vector compareTo" in {
    "dense compare" in {
      vec123a.compareTo(vec124a) must_== -1
      vec124a.compareTo(vec123a) must_==  1
    }
    "sparse compare" in {
      vec1N3a.compareTo(vec123a) must_== -1
      vec123a.compareTo(vec1N3a) must_==  1
    }
    "variable length" in {
      vec12_a.compareTo(vec12Na) must_==  0
    }
  }

  "Vector compareToIgnoreUnset" in {
    "dense compare" in {
      vec123a.compareToIgnoreUnset(vec124a) must_== -1
      vec124a.compareToIgnoreUnset(vec123a) must_==  1
    }
    "sparse compare" in {
      vec1N3a.compareToIgnoreUnset(vec123a) must_==  0
    }
    "variable length" in {
      vec12_a.compareToIgnoreUnset(vec12Na) must_==  0
    }
  }

  "Vector min" in {
    "dense min" in {
      vec123a.min(vec321a) must_== vec121a
      vec321a.min(vec123a) must_== vec121a
    }
    "same instance check" in {
      (vec121a.min(vec123a) eq vec121a) must_== true
      (vec123a.min(vec121a) eq vec121a) must_== true
    }
    "sparse min" in {
      vec1N3a.min(vec321a) must_== vec121a
      vec321a.min(vec1N3a) must_== vec121a
    }
    "variable length min" in {
      vec12_a.min(vec321a) must_== vec121a
      vec321a.min(vec12_a) must_== vec121a
    }
    "sparse max without merge" in {
      vec321a.min(vec1N3a,false) must_== vec1N1a
      vec1N3a.min(vec321a,false) must_== vec1N1a
    }
    "variable length max without merge" in {
      vec12_a.min(vec321a,false) must_== vec12_a
      vec321a.min(vec12_a,false) must_== vec12_a
    }
  }

  "Vector max" in {
    "dense max" in {
      vec123a.max(vec321a) must_== vec323a
      vec321a.max(vec123a) must_== vec323a
    }
    "sparse max" in {
      vec1N3a.max(vec321a) must_== vec323a
      vec321a.max(vec1N3a) must_== vec323a
    }
    "variable length max" in {
      vec12_a.max(vec321a) must_== vec321a
      vec321a.max(vec12_a) must_== vec321a
    }
    "sparse max without merge" in {
      vec321a.max(vec1N3a,false) must_== vec3N3a
      vec1N3a.max(vec321a,false) must_== vec3N3a
    }
    "variable length max without merge" in {
      vec12_a.max(vec321a,false) must_== vec32_a
      vec321a.max(vec12_a,false) must_== vec32_a
    }
  }

}

