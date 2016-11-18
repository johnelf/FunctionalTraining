package com.rea.recursion


// Taken from http://tmorris.net/posts/scala-exercises-for-beginners/index.html

/**
  * Ok here are the rules.
  *
  * You can't use any of the standard list functions, like `map`, `filter`, `flatMap`, `append`, `:::`, `:+`, etc.
  *
  * But you can always use `::` to construct a new list by prepending an element to another list.
  *
  * You CAN and are encouraged to use the solutions from the exercises below to solve the harder
  * ones towards the end.
  *
  * Keep an eye out for repetition and similarities between your answers.
  *
  * REMEMBER: Follow the types, they almost always guide you to the solution.  If it compiles and looks a little
  * too simple, it's probably correct.  As Sherlock Holmes once said, "Each one is suggestive, together they are
  * most certainly conclusive."
  *
  * See if you can make your solution tail recursive, where possible.
  *
  */
object RecursionExercises1 {

  def plusOne(n: Int) = n + 1

  def minusOne(n: Int) = n - 1

  // Add two non-negative Integers together.  You are only allowed to use plusOne and minusOne above
  def add(a: Int, b: Int): Int = {
//    var result = a
//    for (i <- 1 to b) {
//      result = plusOne(result)
//    }
//    result
    if (b == 0)
      a
    else
      add(plusOne(a), minusOne(b))
  }

  // You are not permitted to use any list functions such as map, flatMap, ++, flatten etc
  def sum(l: List[Int]): Int = {
//    var result = 0
//    for (i <- l)
//      result = add(result, i)
//    result
    if(l.length == 1)
      l.head
    else
      add(l.head,sum(l.tail))
  }

  def sum2(l: List[Int]): Int = l match {
    case a::nil => add(a, sum(nil))
    case a::Nil => a
  }

  //Again no list functions are permitted for the following
  def length[A](x: List[A]): Int = x match {
    case Nil=> 0
    case head::Nil=> 1
    case head::tail=> plusOne(length(tail))
  }

  // Do you notice anything similar between sum and length? Hmm...

  // Mapping over a list.  You are given a List of type A and a function converting an A to a B
  // and you give back a list of type B.  No list functions allowed!
  def map[A, B](x: List[A], f: A => B): List[B] = x match {
    case Nil => Nil
    case head::tail => f(head)::map(tail, f)
  }

  // Given a function from A => Boolean, return a list with only those item where the function returned true.
  def filter[A](x: List[A], f: A => Boolean): List[A] = x match {
    case Nil => Nil
    case head::tail if f(head) => head::filter(tail, f)
    case head::tail => filter(tail, f)
  }

  // This pattern should be familiar by now... psst... look at add.
  def append[A](x: List[A], y: List[A]): List[A] = x match {
    case Nil => y
    case pre:+last => append(pre, last::y)
  }
  // Flatten a list of lists to a single list.  Remember you can't use list.flatten.  Can you use a previous
  // solution to solve this one?
  def flatten[A](x: List[List[A]]): List[A] = x match {
    case Nil => Nil
    case head::tail =>append(head,flatten(tail))
  }

  // Follow the types.  You've done a great job getting here. Follow the types.
  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = {
    flatten(map(x, f))
  }

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = x match {
    case Nil => 0
    case head::Nil => head
    case head::tail if head > maximum(tail) => head
    case head::tail => maximum(tail)
  }

  // Reverse a list
  def reverse[A](x: List[A]): List[A] = x match {
    case Nil => Nil
    case head::tail => reverse(tail):+head
  }

}
