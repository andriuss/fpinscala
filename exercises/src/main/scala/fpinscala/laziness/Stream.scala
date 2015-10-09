package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }


  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =  this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (!p(h())) false else t().forAll(p)
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) Cons(() => a, () => acc) else Empty)

  def takeWhile3(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, acc) => if (p(a)) Stream.cons(a, acc) else Empty)

  def headOption: Option[A] = this.foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, acc) => if (f(a)) cons(a, acc) else acc)

  def append[AA >: A](s: Stream[AA]): Stream[AA] =
    foldRight[Stream[AA]](s)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((a, acc) => f(a).append(acc))


  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def take2(n: Int): Stream[A] = unfold((n, this)) {
    case (x, Cons(h, t)) if x > 0 => Some(h(), (x - 1, t()))
    case _ => None
  }

  def takeWhile4(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).forAll {
      case (a, b) => a == b || b.isEmpty
    }

  def tails: Stream[Stream[A]] =
    unfold(this, false) {
      case (Cons(h, t), false) => Some(cons(h(), t()), (t(), t() == Empty))
      case (_, true) => Some(Empty, (Empty, false))
      case _ => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(h, t) => cons(foldRight(z)(f), t().scanRight(z)(f))
    case _ => cons(z, Empty)
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //0, 1, 1, 2, 3, 5, 8
  def fibs: Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] = cons(n1, loop(n2, n1 + n2))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map {
      case (a, s) => cons(a, unfold(s)(f))
    }.getOrElse(Empty)

  def fibs2: Stream[Int] =
    unfold(List(0, 1))((x) => Some((x.head, x.tail :+ x.take(2).sum)))

  def from2(n: Int): Stream[Int] =
    unfold(n)((x) => Some(x, x + 1))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val ones2 = unfold(1)(_ => Some(1, 1))

}

object TestStream {
  def main(args: Array[String]) = {
    val stream = Stream(1, 2, 3, 4)

    println("stream: " + stream)
    println("stream: " + stream.find(_ == 2))

    println("toList: " + stream.toList)
    println("toList: " + Stream(1).toList)
    println("toList: " + Stream().toList)

    println("take: " + stream.take(2).toList)
    println("take: " + stream.take(1).toList)
    println("take: " + stream.take(3).toList)
    println("take: " + stream.take(5).toList)
    println("take: " + Stream().take(2).toList)
    println("take: " + Stream(1).take(1).toList)

    println("drop: " + stream.drop(2).toList)
    println("drop: " + stream.drop(1).toList)
    println("drop: " + stream.drop(3).toList)
    println("drop: " + stream.drop(5).toList)
    println("drop: " + Stream().drop(2).toList)

    println("takeWhile: " + stream.takeWhile(_ < 3).toList)
    println("takeWhile: " + stream.takeWhile(_ < 4).toList)
    println("takeWhile: " + stream.takeWhile(_ == 5).toList)

    println("forAll: " + stream.forAll(_ < 3))
    println("forAll: " + stream.forAll(_ < 4))
    println("forAll: " + stream.forAll(_ > 0))

    println("takeWhile2: " + stream.takeWhile2(_ < 3).toList)
    println("takeWhile2: " + stream.takeWhile2(_ < 4).toList)
    println("takeWhile2: " + stream.takeWhile2(_ == 5).toList)

    println("headOption: " + stream.headOption)
    println("headOption: " + Stream(1).headOption)
    println("headOption: " + Stream(3,4).headOption)
    println("headOption: " + Stream().headOption)

    println("map: " + stream.map(_ + 1).toList)
    println("map: " + stream.map(_ * 2).toList)
    println("map: " + Stream[Int]().map(_ * 2).toList)

    println("filter: " + stream.filter(_ > 2).toList)
    println("filter: " + stream.filter(_ == 2).toList)
    println("filter: " + stream.filter(_ % 2 == 0).toList)

    println("append: " + stream.append(stream).toList)
    println("append: " + stream.append(Stream(5,6)).toList)
    println("append: " + stream.append(Stream()).toList)
    println("append: " + Stream().append(Stream()).toList)
    println("append: " + Stream[Int]().append(Stream(10)).toList)

    println("flatMap: " + stream.flatMap(Stream(_ )).toList)
    println("flatMap: " + stream.flatMap(x => Stream(x + 1)).toList)
    println("flatMap: " + stream.flatMap(x => Stream(x * 2)).toList)

    println("first: " + stream.take(2).asInstanceOf[Cons[Int]].h())

    println("ones:" + ones.take(3).toList)
    println("ones:" + ones.exists(_ % 2 != 0))
    println("ones:" + ones.map(_ + 1).exists(_ % 2 == 0))
    println("ones:" + ones.takeWhile(_ == 1))
    println("ones:" + ones.forAll(_ != 1))

    println("constant: " + constant(1).take(5).toList)
    println("from: " + from(1).take(5).toList)
    println("from: " + from(10).take(5).toList)

    println("fibs: " + fibs.take(10).toList)

    println("unfold: " + unfold(0)((x) => if (x < 5) Some(x, x + 2) else None).toList)
    println("unfold: " + unfold(0)((x) => None).take(2).toList)

    println("fibs2: " + fibs2.take(10).toList)

    println("from2: " + from2(1).take(5).toList)
    println("from2: " + from2(10).take(5).toList)

    println("constant2: " + constant2(1).take(5).toList)

    println("ones2: " + ones2.take(5).toList)

    println("map2: " + stream.map2(_ + 1).toList)
    println("map2: " + stream.map2(_ * 2).toList)
    println("map2: " + Stream[Int]().map2(_ * 2).toList)

    println("take2: " + stream.take2(2).toList)
    println("take2: " + stream.take2(1).toList)
    println("take2: " + stream.take2(3).toList)
    println("take2: " + stream.take2(5).toList)
    println("take2: " + Stream().take2(2).toList)

    println("takeWhile4: " + stream.takeWhile4(_ < 3).toList)
    println("takeWhile4: " + stream.takeWhile4(_ < 4).toList)
    println("takeWhile4: " + stream.takeWhile4(_ == 5).toList)

    println("zipWith: " + stream.zipWith(stream)(_ + _).take(3).toList)
    println("zipWith: " + stream.zipWith(stream)(_ + _).toList)
    println("zipWith: " + stream.zipWith(Stream(): Stream[Int])(_ + _).toList)
    println("zipWith: " + stream.zipWith(Stream(10))(_ + _).toList)

    println("zipAll: " + stream.zipAll(stream).toList)
    println("zipAll: " + stream.zipAll(Stream(): Stream[Int]).toList)
    println("zipAll: " + stream.zipAll(Stream(10, 20)).toList)

    println("startsWith: " + stream.startsWith(stream))
    println("startsWith: " + stream.startsWith(Stream(1,2)))
    println("startsWith: " + stream.startsWith(Stream(1,2,4)))
    println("startsWith: " + stream.startsWith(Stream(2)))
    println("startsWith: " + stream.startsWith(Stream()))
    println("startsWith: " + Stream().startsWith(Stream()))

    println("tails: " + stream.tails.map(_.toList).toList)
    println("tails: " + Stream(1,2,3).tails.map(_.toList).toList)
    println("tails: " + Stream().tails.map(_.toList).toList)

    println("hasSubsequence: " + Stream(1,2,3).hasSubsequence(Stream(2,3)))
    println("hasSubsequence: " + Stream(1,2,3).hasSubsequence(Stream(2,3,4)))

    println("scanRight: " + Stream(1,2,3).scanRight(0)(_ + _).toList)

  }
}