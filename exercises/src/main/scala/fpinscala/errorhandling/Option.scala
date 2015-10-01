package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(x => f(x)).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    //val m: Option[B] = this.map(x => x)
    //val n: Option[B] = m.getOrElse(ob)
    //n
    this match {
      case Some(v) => Some(v)
      case None => ob
    }
  }

  def filter(f: A => Boolean): Option[A] = this.flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(mm => mean(xs.map(x => math.pow(x - mm, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = sys.error("todo")

  def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}

object TestOption {
  import fpinscala.errorhandling.Option._

  def main(args: Array[String]): Unit = {

    val o1: Option[String] = Some("1")
    val o2: Option[String] = None

    println("o1 map: " + o1.map(_ + "_"))
    println("o2 map: " + o2.map(_ + "_"))

    println("o1 getOrElse: " + o1.getOrElse("_"))
    println("o2 getOrElse: " + o2.getOrElse("_"))

    println("o1 flatMap: " + o1.flatMap(x => Some("_")))
    println("o2 flatMap: " + o2.flatMap(x => Some("_")))

    println("o1 orElse: " + o1.orElse(Some("_")))
    println("o2 orElse: " + o2.orElse(Some("_")))

    println("o1 filter: " + o1.filter(_ == "1"))
    println("o1 filter: " + o1.filter(_ == "2"))
    println("o2 filter: " + o2.filter(_ == "1"))

    val list = List(1.0,2.0,3.0)
    println("mean list: " + mean(list))
    println("mean list: " + mean(List()))

    println("variance list: " + variance(list))
    println("variance list: " + variance(List()))

  }
}