package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (if (a < b) a else b)
  }
  
  
  
  property("should be empty after 1 insert and min delete") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }
  
  property("getting minimums should be ascending sequence") = forAll { a: H =>
    isAscending(a)
  }
  
  def isAscending(h: H): Boolean = {
    // if empty or just 1 element then true
    if(isEmpty(h)) return true
    else if(isEmpty(deleteMin(h))) true
    //if previous min is bigger than next min then property "min first" doesn't hold
    else if(findMin(h) > findMin(deleteMin(h))) false
    else isAscending(deleteMin(h))
  }
  
  property("minimum of merge should be also minimum in one of merged heaps") = forAll { (a: H, b: H) =>
    if(isEmpty(a) || isEmpty(b)) true
    else{
    	val m = meld(a, b)
    	findMin(m) == (if (findMin(a) < findMin(b)) findMin(a) else findMin(b))
    }
  }
  
  property("meld with empty should have same minimum") = forAll { a: H =>
    if(isEmpty(a)) true
    else {
     val h = meld(a, empty)
     findMin(h) == findMin(a)
    }
  }
  
  property("melded size should be equal to sum of sizes") = forAll { (a: H, b: H) =>
    val aa = size(a)
    val bb = size(b)
    val cc = size(meld(a, b))
    cc == aa + bb
  }
  
  property("poping meld should be the same as popping from melded minimum") = forAll { (a: H, b: H) =>
    val m = meld(a, b)
    popMeldAndMelders(a, b, m)
  }
  
  def popMeldAndMelders(a: H, b: H, c: H): Boolean = {
    if(isEmpty(c) && isEmpty(a) && isEmpty(b)) true
    else if(isEmpty(c) && (!isEmpty(a) || !isEmpty(b))) false
    else if(isEmpty(a) && isEmpty(b) && !isEmpty(c)) false
    else{
      if(isEmpty(a)){
        if(findMin(b) == findMin(c)) popMeldAndMelders(a, deleteMin(b), deleteMin(c))
        else false
      } 
      else if(isEmpty(b)){
        if(findMin(a) == findMin(c)) popMeldAndMelders(deleteMin(a), b , deleteMin(c))
        else false
      }
      else{
    	  val amin = findMin(a)
			  val bmin = findMin(b)
			  
			  if(amin < bmin) {
          if(findMin(a) == findMin(c)) popMeldAndMelders(deleteMin(a), b , deleteMin(c))
          else false
        }else {
          if(findMin(b) == findMin(c)) popMeldAndMelders(a, deleteMin(b), deleteMin(c))
          else false
         } 
      }
    }
  }
  
  def size(a: H): Int = {
    if(isEmpty(a)) 0
    else 1 + size(deleteMin(a))
  }
  


  lazy val genHeap: Gen[H] = for {
	 more <- arbitrary[Boolean]
   heap <- if(more) nonEmptyHeap else const(empty)
  } yield heap
  
  def nonEmptyHeap: Gen[H] = for {
    head <- arbitrary[Int]
    tail <- genHeap
  } yield insert(head, tail)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
