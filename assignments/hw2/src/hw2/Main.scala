package hw2
import hw2.Data.{B0, BMTree, _}

import scala.annotation.tailrec
import scala.util.control.TailCalls._
// import scala.collection.*

object Main {
  /**
   * Implement given functions, which are currently left blank. (???)
   * **WARNING: Please read the restrictions below carefully.**
   *
   * If you do not follow these, **your submission will not be graded.**
   *
   * 1. Do not use the keyword `var`. Use `val` and `def` instead.
   * 2. From now on, you can use some classes in scala.collections.* (except scala.collections.mutable.*). 
   *    You can also make your custom case classes.
   *
   * Again, your score will be zero if you do not follow these rules.
   *
   * Note that these rules will be gradually relaxed through the next assignments.
   *
   * WARNING: Do not modify Data.scala.
   * We will grade your assignment only with the submitted Main.scala file.
   *
   */

  /**
   * Problem 1: Structural subtype (5 points)
   *
   * Find the **least** (i.e. most specific) common supertype of Ty1 and Ty2.
   *
   *   `CommonTy >: Ty1 && CommonTy >: Ty2`
   *
   * We will check your answer by comparing it with our answer as follows:
   *
   * `checkType(Ty1 <: CommonTy && Ty2 <: CommonTy && CommonTy <: Answer)`
   *
   * DO NOT USE "Any" in anywhere in your code
   */
  object Problem1 {
    class MyClass[A, B, C, D, E, F]() {
      type Func1 = {val a: B} => {val b: A}
      type Func2 = {val b: D} => {val a: A}
      type Func3 = {val c: C} => {val a: B}
      type Func4 = {val f: E} => {val d: F}

      type Ty1 = {
        def apply: {val func: Func1; val c: C} => {val b: B; val c: C; val f: F}
        def function1: {val func: Func3} => {val a: A; val func: Func2}
        val a: A
        val b: B
        val f: F
      }

      type Ty2 = {
        def apply: {val func: Func2; val e: E} => {val b: B; val e: E }
        def function1: {val func: Func4} => {val c: C; val func: Func1}
        val a: A
        val c: C
        val d: D
      }

      /**
       * WRITE YOUR ANSWER
       */
      type CommonTy = {
        def apply: {val func: Func1}
        def function1: {val func: Func3; val a: A}
        val a: A
      }
    }
  }


  /**
   * Problem 2: Reversed Binary Representation (5 points each)
   *
   * Implement the basic operations of reversed binary representation (RBB).
   * You should use the predefined `BNum` type from Data.scala file.
   *
   * RBB is one way to represent natural number through ADT.
   *
   * BHead: Most significant bit (1)
   * B1: One (1)
   * B0: Zero (0)
   *
   * e.g.)
   * number 1 -> BHead
   * number 6 -> /binarize/ 110 (2) -> /reverse/ 011 -> /RBB/ B0(B1(BHead))
   * number 11 -> /binarize/ 1011 (2) -> /reverse/ 1101 -> /RBB/ B1(B1(B0(BHead)))
   **/

  /**
   * Addition (a + b)
   * e.g.) add(B0(B1(BHead)), B0(BHead)) == B0(B0(B0(BHead))) // 6 + 2 = 8
   */
  // for the problem 2
//  sealed trait BNum
//  case object BHead extends BNum
//  case class B0(next: BNum) extends BNum
//  case class B1(next: BNum) extends BNum
//  object BHead{
//    = 1
//  }

  def add(a: BNum, b: BNum): BNum = {
    def addCarry(res: BNum): BNum = {
      res match {
        case BHead => B0(BHead)
        case B0(next) => B1(next)
        case B1(next) => B0(addCarry(next))
      }
    }

    (a, b) match{
      // BHead
      case (BHead, BHead) => B0(BHead) // 1 + 1
      case (BHead, B0(BHead)) => addCarry(b) //
      case (B0(BHead), BHead) => addCarry(a)
      case (BHead, B1(BHead)) => addCarry(b) //
      case (B1(BHead), BHead) => addCarry(a) //

      // Recursive operations
      case (B0(aNext), B0(bNext)) => B0(add(aNext, bNext))
      case (B1(aNext), B0(bNext)) => B1(add(aNext, bNext))
      case (B0(aNext), B1(bNext)) => B1(add(aNext, bNext))
      case (B1(aNext), B1(bNext)) => B0(add(addCarry(aNext), bNext))
    }
  }

  /**
   * Multiplication (a * b)
   * e.g.) mul(B0(B1(BHead)), B0(BHead)) == B0(B0(B1(BHead))) // 6 * 2 = 12
   */
  def find_value(x: BNum): Int = {
    x match {
      case BHead => 1
      case B0(some) => 0 + 2 * (find_value(some))
      case B1(some) => 1 + 2 * (find_value(some))
    }
  }
  def mul(a: BNum, b: BNum): BNum = {
    val comp = compare(a,b)
    def sumNtimes(times: Int, res: BNum): BNum ={
      if(times == 0) res
      else
        if(comp == 1) sumNtimes(times - 1, add(res, a))
        else sumNtimes(times - 1, add(res, b))
    }

    if(comp == 1){
      val b_value = find_value(b)
      sumNtimes(b_value - 1, a)
    }else{
      val a_value = find_value(a)
      sumNtimes(a_value - 1, b)
    }
  }

  /**
   * Comparison (a > b)
   * e.g.) compare(B0(B1(BHead)), B0(BHead)) == 1 // 6 > 2
   *
   * return
   * | 1  (a > b)
   * | 0  (a == b)
   * | -1 (a < b)
   */
  def compare(a: BNum, b: BNum): Int = {
    val a_val = find_value(a)
    val b_val = find_value(b)

    if(a_val > b_val) 1
    else if (a_val == b_val)0
    else -1
  }

  /**
   * Problem 3: Binomial Heap (5 points each)
   *
   * Implement the basic operations of binomial heap.
   * https://en.wikipedia.org/wiki/Binomial_heap
   *
   * We will grade your score by checking the result heap satisfies the binomial heap conditions:
   * 1. Every binomial tree in a heap obeys the minimum-heap property:
   *    the value of a node is greater than or equal to the value of its parent.
   * 2. There can be at most one binomial tree for each order, including zero order.
   * 3. findMin and deleteMin correctly return the minimum value.
   *
   * Which means, you don't need to concern the exact order of the values in your heap.
   *
   * WARNING: You should not confuse it with *Binary* heap.
   *
   * cf) We can use type restriction on T that T is comparable.
   *     That feature will be presented on the latter classes.
   **/
//  case class BMTree[T](value: T, order: Int, children: List[BMTree[T]])
//
//  case class BMHeap[T](trees: List[BMTree[T]])
//    def merge_binary_tree[T <: BNum](bt1: BMTree[T], bt2: BMTree[T]): BMTree[BNum] ={
//      // Greater
//      if(compare(bt1.value, bt2.value) >= 0){
//        val new_tree: BMTree[BNum] = new BMTree[BNum](bt1.value, bt1.order, bt2 :: bt1.children)
//        new_tree
//      }else{
//        val new_tree: BMTree[BNum] = new BMTree[BNum](bt2.value, bt2.order, bt1 :: bt2.children)
//        new_tree
//      }
//    }
//  def merge[T <: BNum](left: BMHeap[T], right: BMHeap[T], compare: (T, T) => Int): BMHeap[T] = {
//
//    if(left.trees.)
//  }

  def insert[T](heap: BMHeap[T], value: T, compare: (T, T) => Int): BMHeap[T] = ???

  // return Some(minimum value) if the heap is not empty
  // return None if the heap is empty
  def findMin[T](heap: BMHeap[T], compare: (T, T) => Int): Option[T] = ???

  // return (Some(minimum value), remained heap) if the heap is not empty
  // return (None, empty heap) if the heap is empty
  def deleteMin[T](heap: BMHeap[T], compare: (T, T) => Int): (Option[T], BMHeap[T]) = ???
}
