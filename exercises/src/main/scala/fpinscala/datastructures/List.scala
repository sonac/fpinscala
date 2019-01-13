package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => List(h)
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (!f(x)) l else dropWhile(xs, f)
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, x: Int) => x + 1)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def length3[A](l: List[A]): Int = {
    foldLeft(l, 0)((x: Int, _) => x + 1)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((a, b) => Cons(b, a))

  def append2[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def flatten[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => x
    case Cons(x, xs) => append(x, flatten(xs))
  }

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    flatten(map(l)(f))
  }

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4)
    val listD = List(1.0, 2.0, 3.0, 4.0)
    val listoflist = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    println(3 == List.x)
    println(tail(list))
    println(setHead(list, 4))
    println(drop(list, 2))
    println(dropWhile(list, (x : Int) => x < 2))
    println(init(list))
    println(length(list))
    println(foldLeft(list, 1)(_ * _))
    println(length3(list))
    println(sum3(list))
    println(product3(listD))
    println(reverse(list))
    println(append2(list, list))
    println(flatten(listoflist))
    println(addOne(list))
    println(doubleToString(listD))
    println(map(list){x: Int => x * 2})
    println(filter(list)({x: Int => x % 2 == 0}))
    println(flatMap(list)({x: Int => List(x, x * 2)}))
  }
}
