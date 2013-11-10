package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.collection.immutable.SortedSet

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    if (a < b) findMin(h) == a else findMin(h) == b
  }
  
  property("emptied") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }
  
  property("sorted") = forAll { (h : H) =>
  	valuesSorted(Int.MinValue, h)
  }
  
  property("minMelded") = forAll { (h1 : H, h2 : H) =>
  	val x1 = findMin(h1)
  	val x2 = findMin(h2)
  	val xm = findMin(meld(h1, h2))
  	if (x1 <= x2) xm == x1 else xm == x2
  }
  
  property("sortedMelded") = forAll { (h1 : H, h2 : H) =>
  	valuesSorted(Int.MinValue, meld(h1, h2))
  }
  
  property("valuesMelded") = forAll { (h1 : H, h2 : H) =>
  	(values(h1) ++ values(h2)) == values(meld(h1, h2))
  }
  
  def values(h : H) : Set[Int] = isEmpty(h) match {
    case true => Set()
    case false => values(deleteMin(h)) + findMin(h)
  }
  
  def valuesSorted(x: Int, h: H): Boolean = isEmpty(h) match {
  	case true => true
    case false =>
      val y = findMin(h)
      if (x <= y) valuesSorted(y, deleteMin(h)) 
      else false
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
