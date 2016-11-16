package com.rea.collections

import com.rea.collections.Collections._
import org.specs2.mutable.Specification

class CollectionsSpec extends Specification {

  "Collections" should {
    "return last element" in {
      last(List(1,2,3,4,5)) === 5
    }

    //By convention, the first element in the list is element 0.
    "Find the Kth element of a list." in {
      nth(2, List(1,2,3,4,5)) === 2
    }

    "reverse the list" in {
      reverse(List(1,2,3,4,5)) === List(5,4,3,2,1)
    }

    "flatten the list" in {
      flatten(List(List(1), List(2), List(3))) === List(1,2,3)
    }

    "flatten the list that contains multiple numbers" in {
      flatten(List(List(1), List(2,22,222), List(3,2))) === List(1,2,22,222,3,2)
    }

  }

}
