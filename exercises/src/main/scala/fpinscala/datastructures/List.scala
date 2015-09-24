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

  def appendFL[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((x, y) => Cons(y, x))
  def appendFR[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(x, concat(xs))
  }

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def toString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, toString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }


  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

  def merge(as1: List[Int], as2: List[Int]): List[Int] = (as1, as2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, merge(xs, ys))
    case (Cons(x, xs), Nil) => Cons(x, xs)
    case (Nil, Cons(x, xs)) => Cons(x, xs)
  }

  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = (as1, as2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case (Cons(x, xs), Nil) => Cons(x, xs)
    case (Nil, Cons(x, xs)) => Cons(x, xs)
  }

  def zipWith2[A,B,C](as1: List[A], as2: List[B])(f: (A, B) => C): List[C] = (as1, as2) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith2(xs, ys)(f))
    case _ => Nil
  }

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
    println("concat: " + List.concat(List(Nil, List(1, 2), List(3))))

    println("addOne: " + List.addOne(list))
    println("toString: " + List.toString(List(1.0, 2.0, 3.0)))
    println("map: " + List.map(list)(_ + 2))

    println("filter: " + List.filter(list)(_ == 2))
    println("filter: " + List.filter(list)(_ > 1))
    println("filter: " + List.filterFM(list)(_ == 2))
    println("filter: " + List.filterFM(list)(_ > 1))

    println("flatMap: " + List.flatMap(list)(x => List(x, 5)))
    println("flatMap: " + List.flatMap(list)(x => Nil))
    println("flatMap: " + List.flatMap(List(1,2,3))(i => List(i,i)))

    println("merge: " + List.merge(list, list2))
    println("merge: " + List.merge(list, List(4, 5)))
    println("merge: " + List.merge(list, Nil))
    println("merge: " + List.merge(Nil, Nil))

    println("zipWith: " + List.zipWith(list, list2)(_ + _))
    println("zipWith2: " + List.zipWith2(list, list2)(_ + _))
    println("zipWith2: " + List.zipWith2(list, List(10,11))(_ + _))


  }
}
