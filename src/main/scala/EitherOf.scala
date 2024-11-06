package com.latwal



trait EitherOf[+E,+A]{
	def map[B](f: A => B): EitherOf[E, B] =
		this match
			case Left(value) => Left(value)
			case Right(value) => Right(f(value))

	def flatMap[EE >: E, B](f: A => EitherOf[EE, B]): EitherOf[EE, B] =
		this match
			case Left(value) => Left(value)
			case Right(value) => f(value)

	def orElse[EE >: E, B >: A](b: => EitherOf[EE, B]): EitherOf[EE, B] =
		this match
			case Left(value) => b
			case Right(value) => Right(value)

	def map2[EE >: E, B, C](b: EitherOf[EE, B])(f: (A, B) => C): EitherOf[EE, C] =
		for
			a <- this
			b1 <- b
		yield f(a,b1)
}
case class Left[+E](value: E) extends EitherOf[E,Nothing]
case class Right[+A](value: A) extends EitherOf[Nothing,A]

object EitherOf {
	def mean(xs: IndexedSeq[Double]) : EitherOf[String,Double] =
		if(xs.isEmpty) Left("mean of empty list")
		else Right(xs.sum / xs.length)
	
	def safeDiv(x: Int, y: Int): EitherOf[Exception,Int] =
		Try(x/y)
	
	def Try[A](a: => A): EitherOf[Exception,A] =
		try Right(a)
		catch
			case e : Exception => Left(e)

	def insuranceRateQuote(
		age: Int,
		numberOfSpeedingTickets: Int
	): Int = age * numberOfSpeedingTickets

	def parseInsuranceRateQuote(
		age: String,
		numberOfSpeedingTickets: String
	) : EitherOf[Exception,Int] =
		for {
			a <- Try { age.toInt }
			tickets <- Try { numberOfSpeedingTickets.toInt }
		} yield insuranceRateQuote(a,tickets)

	def traverse[E, A, B](as: List[A])(
		f: A => EitherOf[E, B]): EitherOf[E, List[B]] = {
		as.foldLeft(Right(Nil) : EitherOf[E,List[B]])(
			(a,b) => a.map2(f(b))((x,y) => y :: x)
		)
	}

	def sequence[E, A](es: List[EitherOf[E, A]]): EitherOf[E, List[A]] =
		traverse(es)(x => x)


}