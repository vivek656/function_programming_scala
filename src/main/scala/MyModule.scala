package com.latwal

object MyModule {

  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."

    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int = 0, b: Int = 1): Int = {
      if (n == 0) a
      else if (n == 1) b
      else go(n - 1, b, a + b)
    }

    go(n)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def partiall[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C) : (A,B) => C =
    (a:A, b:B) => f(a)(b)

  def compose[A,B,C](f : B => C , g : A => B) : A => C =
    (a:A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibanacci", 4, fib))
    println(
      findFirst(
        Array(7, 9, 10), (x: Int) => x == 9
      )
    )
  }
}