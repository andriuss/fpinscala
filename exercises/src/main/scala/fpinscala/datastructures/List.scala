package fpinscala.datastructures

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

  val list = List(1,2,3,4,5) match {
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


  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => l // or throw error
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => l // or throw error
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l // or throw error
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    def loop(left: List[A], ls: List[A]): List[A] = left match {
      case Nil => Nil
      case Cons(_, Nil) => ls
      case Cons(x, xs) => loop(xs, append(ls, Cons(x, Nil)))
    }
    loop(l, Nil)
  }


  def length[A](l: List[A]): Int = foldRight[A, Int](l, 0)((x: A, y: Int) => y + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumFL(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

  def productFL(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int = foldLeft[A, Int](l, 0)((x: Int, y: A) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def foldLeftFR[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)((x, y) => f(y, x))
  def foldRightFL[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, z)((x, y) => f(y, x))

  def appendFL[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((x: List[A], y: A) => Cons(y, x))
  def appendFR[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    //case Cons(x, Nil) => x
    case Cons(x, Cons(y, xs)) => append[A](x, y)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

}

object Test {
  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3)
    val list2 = List(4, 5, 6)
    val ll = List(List(1, 2), List(3, 4), List(5))

    println("tail: " + List.tail(list))

    println("drop(1): " + List.drop(list, 1))
    println("drop(2): " + List.drop(list, 2))

    println("dropWhile: " + List.dropWhile[Int](list, _ < 2))
    println("dropWhile: " + List.dropWhile[Int](list, _ < 3))
    println("dropWhile: " + List.dropWhile[Int](list, _ < 4))

    println("init: " + List.init(list))
    println("init: " + List.init(List()))
    println("init: " + List.init(List(1)))
    println("init: " + List.init(List(1, 2)))

    println("test: " + List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    println("length: "   + List.length(list))
    println("lengthFL: " + List.lengthFL(list))
    println("length: " + List.length(Nil))
    println("length: " + List.length(List("1")))

    //println("length of 10 000: " + List.length(List((1 to 10000): _*)))
    println("foldLeft: " + List.foldLeft(list, 0)(_ + _))
    println("foldLeft: " + List.foldLeft(Nil: List[Int], 0)(_ + _))

    println("reverse: " + List.reverse(list))

    println("foldLeftFR: " + List.foldLeftFR(list, 0)(_ + _))
    println("foldRightFL: " + List.foldRightFL(list, 0)(_ + _))

    println("appendFL: " + List.appendFL(list, list2))
    println("appendFR: " + List.appendFR(list, list2))

    println("concat: " + List.concat(ll))


  }
}
