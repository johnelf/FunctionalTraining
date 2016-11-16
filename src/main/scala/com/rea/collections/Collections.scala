package com.rea.collections

import scala.annotation.tailrec

object Collections {

  /*
    In Java:
      for (int i=0; i < max; i++) {
        doStuff(i);
      }
    In Scala:
      // Index counting equivalent
      for (i <- 0 until max) {
        doStuff(i)
      }

      // Obtain index alongside item with combinators
      for ((item, i) <- myList.zipWithIndex) {
        doStuff(item, i)
      }

    Scala list有head, tail两个方法, 比如: list.head取得第一个元素, list.tail返回除第一个元素外剩下的list
   */
  @tailrec
  def last(xs: List[Int]): Int = {
//    xs(xs.length -1)
    if (xs.length == 1) return xs.head
    last(xs.tail)
  }

  def nth(number: Int, xs: List[Int]): Int = {
    xs(number-1)
  }

  def reverse(xs: List[Int]): List[Int] = {
    if (xs.length == 1) return xs
    reverse(xs.tail) :+ xs.head
  }

  def flatten(xs: List[List[Int]]): List[Int] = {
    xs match {
      case head::Nil => head
      case head::tail => head ++ flatten(tail)
    }
//    if (xs.length == 1) return xs.head
//    xs.head ++ flatten(xs.tail)
//    var newList = List()
//    xs.foldLeft[List[Int]](Nil)((acc: List[Int], i: List[Int]) => {acc ++ i})
  }

  def main(args: Array[String]) {
    val list1 = List(1,2,3)
    val list2 = List(4,5,6)

    val result: List[Int] = list1.flatMap(number1 => list2.map(number2 => number2 + number1))

    println(result)
  }

}
