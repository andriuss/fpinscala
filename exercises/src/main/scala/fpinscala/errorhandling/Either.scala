package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case Left(a) => Left(a)
 }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(a) => Left(a)
  }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => Right(a)
   case Left(_) => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   this.flatMap(a => b.map(bb => f(a, bb)))

  def map3[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[Seq[EE], C] = (this, b) match {
    case (Right(x), Right(y)) => Right(f(x,y))
    case (Left(x), Right(_)) => Left(Seq(x))
    case (Right(_), Left(y)) => Left(Seq(y))
    case (Left(x), Left(y)) => Left(Seq(x,y))
  }

  def map4[EE >: E, B, C, D](b: Either[EE, B])(f: (A, B) => C)(ef: Seq[EE] => D): Either[D, C] = (this, b) match {
    case (Right(x), Right(y)) => Right(f(x,y))
    case (Left(x), Right(_)) => Left(ef(Seq(x)))
    case (Right(_), Left(y)) => Left(ef(Seq(y)))
    case (Left(x), Left(y)) => Left(ef(Seq(x,y)))
  }

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case x :: xs => f(x).flatMap(a => traverse(xs)(f).map(b => a :: b))
    case Nil => Right(Nil)
  }

  def traverse2[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case x :: xs => f(x).map2(traverse2(xs)(f))(_ :: _)
    case Nil => Right(Nil)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }


  def mkName(name: String): Either[String, Name] = if (name == "" || name == null) Left("Name is empty.") else Right(new Name(name))
  def mkAge(age: Int): Either[String, Age] = if (age < 0) Left("Age is out of range.") else Right(new Age(age))
  def mkPerson(name: String, age: Int): Either[String, Person] = mkName(name).map2(mkAge(age))(Person(_, _))
  def mkPerson2(name: String, age: Int): Either[Seq[String], Person] = mkName(name).map3(mkAge(age))(Person(_, _))
  def mkPerson3(name: String, age: Int): Either[String, Person] = mkName(name).map4(mkAge(age))(Person(_, _))(_.mkString(","))
}

case class Person(name: Name, age: Age)
sealed case class Name(val value: String)
sealed case class Age(val value: Int)

object TestEither {
  import Either._

  def main(args: Array[String]): Unit = {
    println("safeDiv: " + safeDiv(10,2))
    println("safeDiv: " + safeDiv(1,0))


    val left: Either[String, Int] = Left("opps")
    val right: Either[String, Int] = Right(100)

    println("map: " + left.map(_ + 1))
    println("map: " + right.map(_ + 1))

    println("flatMap: " + left.flatMap(x => Right(x + 1)))
    println("flatMap: " + right.flatMap(x => Right(x + 1)))

    println("orElse: " + left.orElse(Right(5)))
    println("orElse: " + right.orElse(Right(5)))

    println("map2: " + left.map2(right)(_ * _))
    println("map2: " + right.map2(left)(_ * _))
    println("map2: " + right.map2(right)(_ * _))

    println("traverse: " + traverse(List(left, right))(x => x))
    println("traverse2: " + traverse2(List(left, right))(x => x))

    println("sequence: " + sequence(List(right, left, right)))

    println {
      for {
        a <- safeDiv(10, 2)
        b <- safeDiv(20, 2)
        //c <- safeDiv(20, 0)
      } yield a + b
    }

    println("mkPerson: " + mkPerson("Andrius", 31))
    println("mkPerson: " + mkPerson("Andrius", -1))
    println("mkPerson: " + mkPerson("", -1))
    println("mkPerson2: " + mkPerson2("", -1))
    println("mkPerson3: " + mkPerson3("", -1))
  }
}