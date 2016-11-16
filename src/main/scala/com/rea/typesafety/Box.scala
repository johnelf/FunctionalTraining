package com.rea.typesafety

case class Box[A](get: A) {
  def map[B](f: A => B): Box[B] = Box(f(get))

  def flatMap[B](f: A => Box[B]): Box[B] = f(get)

}

object Box {
  def join[A](box: Box[Box[A]]): Box[A] = box.get
}

