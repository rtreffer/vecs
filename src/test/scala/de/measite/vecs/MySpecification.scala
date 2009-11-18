package de.measite.vecs

import de.measite.vecs.data._

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

class MySpecTest extends JUnit4(MySpecification)

class MySpecRunner extends ConsoleRunner(MySpecification)

object MySpecification extends Specification {
  name= "Specification"
  "KVector"    isSpecifiedBy KVectorSpec
  "RRectangle" isSpecifiedBy RRectangleSpec
} 
