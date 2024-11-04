package com.latwal

import com.latwal

import scala.annotation.tailrec
import scala.runtime.Nothing$

sealed trait FunctionalList[+A] {
  def head() : A

  def tail(): FunctionalList[A]

  def drop(n: Int): FunctionalList[A]

  def dropWhile(f: A => Boolean): FunctionalList[A]

  def append[B >: A](l: FunctionalList[B]): FunctionalList[B]

  def foldRight[B](z: B)(f: (A, B) => B): B

  def foldLeft[B](z: B)(f: (A, B) => B): B

  def reverse(): FunctionalList[A]

  def map[B](f: (a: A) => B): FunctionalList[B]

  def filter(f: (a: A) => Boolean): FunctionalList[A] = this

  def flatMap[B](f: (a: A) => FunctionalList[B]): FunctionalList[B]
}

case object NilFunctionalList$ extends FunctionalList[Nothing]:
  override def head(): Nothing = throw new UnsupportedOperationException("head of empty list")
  override def tail(): FunctionalList[Nothing] = throw new UnsupportedOperationException("tail of empty list")

  override def drop(n: Int): FunctionalList[Nothing] = throw new UnsupportedOperationException("nothing to drop in  empty list")

  override def dropWhile(f: Nothing => Boolean): FunctionalList[Nothing] = NilFunctionalList$

  override def append[B >: Nothing](l: FunctionalList[B]): FunctionalList[B] = l

  override def foldRight[B](z: B)(f: (Nothing, B) => B): B = z

  override def foldLeft[B](z: B)(f: (Nothing, B) => B): B = z

  override def reverse(): FunctionalList[Nothing] = this

  override def map[B](f: (a: Nothing) => B): FunctionalList[B] = this

  override def filter(f: (a: Nothing) => Boolean): FunctionalList[Nothing] = this

  override def flatMap[B](f: (a: Nothing) => FunctionalList[B]): FunctionalList[B] = this


case class Cons[+A](h: A, t: FunctionalList[A]) extends FunctionalList[A]:
  override def head(): A = h
  override def tail(): FunctionalList[A] = t

  override def drop(n: Int): FunctionalList[A] = (1 to n).foldLeft(this)((b: FunctionalList[A], _) => b match
    case Cons(_, _) => b.tail()
    case NilFunctionalList$ => b
  )

  override def append[B >: A](l: FunctionalList[B]): FunctionalList[B] =
    this match
      case Cons(h, t) => Cons(h, t.append(l))

  override def dropWhile(f: A => Boolean): FunctionalList[A] = {
    @tailrec
    def go(listTest: FunctionalList[A]): FunctionalList[A] = {
      listTest match
        case Cons(a, _) if f(a) => go(listTest.tail())
        case _ => listTest
    }

    go(this)
  }

  /**
   * fold right cannot be tail rec
   */
  def foldRight[B](z: B)(f: (A, B) => B): B =
    this match
      case Cons(x, xs) => f(x, xs.foldRight(z)(f))

  def foldRightNonRec[B](z: B)(f: (A, B) => B): B = {
    val j: FunctionalList[A] = NilFunctionalList$
    val rightFolded = this.foldLeft(j)((a, b) => {
      Cons(a, b)
    })
    rightFolded.foldLeft(z)(f)
  }

  def foldLeft[B](z: B)(f: (A, B) => B): B = {
    @tailrec
    def go(x: FunctionalList[A], acc: B): B = {
      x match
        case NilFunctionalList$ => acc
        case Cons(h, t) => go(t, f(h, acc))
    }

    go(this, z)
  }

  override def reverse(): FunctionalList[A] = {
    val j: FunctionalList[A] = NilFunctionalList$
    this.foldLeft(j)((a, b) => {
      Cons(a, b)
    })
  }

  override def map[B](f: (a: A) => B): FunctionalList[B] = {
    val j: FunctionalList[B] = NilFunctionalList$
    this.foldLeft(j)((a, b) => {
      b match
        case NilFunctionalList$ => Cons(f(a), b)
        case Cons(h, t) => b.append(Cons(f(a), NilFunctionalList$))
    })
  }

  override def filter(f: (a: A) => Boolean): FunctionalList[A] =
    this.foldLeft(NilFunctionalList$: FunctionalList[A])((a, b) => {
      if (f(a)) b.append(Cons(a, NilFunctionalList$))
      else b
    })

  override def flatMap[B](f: (a: A) => FunctionalList[B]): FunctionalList[B] = {
    this.foldLeft(NilFunctionalList$: FunctionalList[B])((a, b) =>
      b.append(f(a))
    )
  }


object FunctionalList {
  def sum(ints: FunctionalList[Int]): Int = ints match {
    case NilFunctionalList$ => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: FunctionalList[Double]): Double = ds match {
    case NilFunctionalList$ => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): FunctionalList[A] =
    if (as.isEmpty) NilFunctionalList$
    else Cons(as.head, apply(as.tail: _*))

  def length[A](as: FunctionalList[A]): Int =
    as match
      case Cons(head, next) => 1 + length(next)
      case NilFunctionalList$ => 0

  def concat[A](list: FunctionalList[A]*): FunctionalList[A] = {
    if (list.isEmpty) return NilFunctionalList$
    val l: FunctionalList[A] = NilFunctionalList$
    list.foldLeft(l) { (b, a) =>
      b.append(a)
    }
  }

  def zipWith[A,B](a1: FunctionalList[A], a2: FunctionalList[A])(f: (a: A, b: A) => B): FunctionalList[B] = {
    var (x,y) = (a1,a2)
    if (length(a1) == 0 || length(a2) == 0) NilFunctionalList$
    else (1 to Math.min(length(a1) , length(a2)))
      .foldLeft(NilFunctionalList$ : FunctionalList[B])((b, a) => {
        val x1 = x.head()
        val y1 = y.head()
        x = x.tail()
        y = y.tail()
        b.append(Cons(f(x1,y1) , NilFunctionalList$))
      })
  }

  def hasSubSequence[A](a : FunctionalList[A], b : FunctionalList[A]): Boolean = {
    Cons(a, b) match
      case Cons(NilFunctionalList$,_) => false
      case Cons(_,NilFunctionalList$) => false
      case Cons(x ,y) =>
        if(length(a) < length(b)) false
        else {
          (a.head() == b.head() || hasSubSequence(a.tail(), b.tail()))
           || hasSubSequence(a.tail(),b)
        }
  }

  def main(args: Array[String]): Unit = {
    /**
     * this constructors work because we implemented
     * [apply] method .
     * variadic apply in companion object let us create
     * these variable constructors
     */
    val x = FunctionalList(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case NilFunctionalList$ => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)

    val xs = FunctionalList(1, 2, 3, 4, 5)
    val ex1 = xs.dropWhile { a =>
      a < 4
    }
    println(ex1)
    val xs2 = FunctionalList(7, 8, 9, 10)
    val xs3 = xs.append(xs2)
    println(xs3)

    val product = xs3.foldRight(1.0)(_ * _)
    println(product)
    val j: FunctionalList[Int] = NilFunctionalList$
    val txt = xs3.foldRight(j)(Cons(_, _))
    println(txt)

    println(length(xs3))

    def plf[T](a: T, b: String) = b + a.toString

    println(xs3.foldLeft("")(plf))
    println(xs3.foldRight("")(plf))
    println(xs3.asInstanceOf[Cons[Any]].foldRightNonRec("")(plf))
    println(concat(xs, xs2))
    println(
      xs3.map(_ + 1).foldLeft("")(plf)
    )
    println(
      xs3.filter(_ % 2 == 0).foldLeft("")(plf)
    )
    println(
      xs3.flatMap(x => FunctionalList(x, x)).foldLeft("")(plf)
    )
    println(
      zipWith(xs,xs2)(_ + _).foldLeft("")(plf)
    )
    val d1 = FunctionalList("a" , "b" , "c" , "d")
    val d2 = FunctionalList("b" , "d")
    println(
      hasSubSequence(d1,d2)
    )

  }
}

