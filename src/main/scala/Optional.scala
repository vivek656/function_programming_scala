package com.latwal

import scala.::


trait Optional[+A] {
	def isEmpty(): Boolean

	def map[B](f: A => B): Optional[B]

	def flatMap[B](f: A => Optional[B]): Optional[B]

	def getOrElse[B >: A](default: => B): B

	def orElse[B >: A](ob: => Optional[B]): Optional[B]

	def filter(f: A => Boolean): Optional[A]
}

case class Some[+A](get: A) extends Optional[A]:
	override def map[B](f: A => B): Optional[B] = Some(f(get))

	override def flatMap[B](f: A => Optional[B]): Optional[B] = f(get).orElse(None)

	override def getOrElse[B >: A](default: => B): B = get

	override def orElse[B >: A](ob: => Optional[B]): Optional[B] = Some(get)

	override def filter(f: A => Boolean): Optional[A] = if (f(get)) this else None

	override def isEmpty(): Boolean = false


case object None extends Optional[Nothing]:
	override def map[B](f: Nothing => B): Optional[B] = None

	override def flatMap[B](f: Nothing => Optional[B]): Optional[B] = None

	override def getOrElse[B >: Nothing](default: => B): B = default

	override def orElse[B >: Nothing](ob: => Optional[B]): Optional[B] = ob

	override def filter(f: Nothing => Boolean): Optional[Nothing] = this

	override def isEmpty(): Boolean = true


object Optional {


	/**
	 * Lift is used to convert a simple a => b (a to b )
	 * to Optional function.
	 * We can use optional classes without duplicating the actual logic.
	 * _ map f is same as (x : Optional[A] => x.map(f) ) which  we need as
	 * Optional.map returns optional.
	 *
	 * in scala compiler automatically infers _  with correct
	 * type from return parameter
	 *
	 */
	def lift[A, B](f: A => B): Optional[A] => Optional[B] = _ map f

	val absO: Optional[Double] => Optional[Double] = lift(math.abs)

	/**
	 * [:=>] is a call by name,
	 * the value or expression is not called until needed
	 * same as lazy.
	 */
	def Try[A](a: => A): Optional[A] =
		try Some(a)
		catch {
			case e: Exception => None    
		}

	def map2_2[A, B, C](a: Optional[A], b: Optional[B])
	                   (f: (A, B) => C): Optional[C] = {
		(a, b) match
			case (Some(aa), Some(bb)) => Try(f(aa, bb))
			case _ => None

	}

	/**
	 *
	 * actual impl from book.
	 */
	def map2[A, B, C](a: Optional[A], b: Optional[B])(f: (A, B) => C):
	Optional[C] =
		a.flatMap(aa =>
			b.map(bb => f(aa, bb))
		)

	/**
	 * equivalent of map2 , scala provide this
	 * for-comprehension
	 * which converts aa <- a to a.flatmap
	 * and bb <- b to b.map
	 * then we can use these values in yield.
	 *
	 * Optional should have a functional called flatMap and map
	 *
	 */
	def map2_1[A, B, C](a: Optional[A], b: Optional[B])(f: (A, B) => C):
	Optional[C] =
		for {
			aa <- a
			bb <- b
		} yield f(aa, bb)


	/**
	 * :: is a function of a list
	 */
	def sequence[A](a: List[Optional[A]]): Optional[List[A]] = {
		traverse(a)(x => x)
	}

	def traverse[A, B](a: List[A])(f: A => Optional[B]): Optional[List[B]] = {
		a.foldLeft(Some(Nil): Optional[List[B]])((a, b) => map2(f(b), a)(_ :: _))
	}

	def variance(xs: Seq[Double]): Optional[Double] = {
		mean(xs).flatMap(
			m => mean(xs.map(x => math.pow(x - m, 2)))
		)
	}

	def mean(xs: Seq[Double]): Optional[Double] = {
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}

	def insuranceRateQuote(
		age: Int,
		numberOfSpeedingTickets: Int
	): Int = age * numberOfSpeedingTickets

	def parseInsuranceRateQuote(
		age: String,
		numberOfSpeedingTickets: String
	): Optional[Double] = {
		val optAge = Try(age.toInt)
		val optTickets = Try(numberOfSpeedingTickets.toInt)
		map2(optAge, optTickets)(insuranceRateQuote)
	}

	def main(args: Array[String]): Unit = {

	}
}