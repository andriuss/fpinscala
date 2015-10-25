package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble/ (Int.MaxValue - 1), rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(n: Int, ls: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n < 1) (ls, rng)
      else {
        val (i, rng2) = rng.nextInt
        go(n -1, ls :+ i, rng2)
      }
    }
    go(count, Nil, rng)
  }

  def double2(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue - 1))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rand => {
      val (a, aRNG) = ra(rand)
      val (b, bRNG) = rb(aRNG)
      (f(a, b), bRNG)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight[Rand[List[A]]](rng => (List(), rng))((n: Rand[A], acc: Rand[List[A]]) => rand => {
      val (a, rand2) = n(rand)
      val (b, rand3) = acc(rand2)
      (a :: b, rand3)
    })
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    val ls: List[Rand[Int]] = List.fill(count)(r => r.nextInt)
    sequence(ls)(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { x =>
      val mod = x % n
      if (x + (x-1) - mod >= 0) r => (mod, r)
      else nonNegativeLessThan(n)
    }
  }

  def nonNegativeLessThanOrig(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => rng => (f(x), rng))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(x => rng => {
      val (a, aRNG) = ra(rng)
      val (b, bRNG) = rb(aRNG)
      (f(a, b), bRNG)
    })

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State(s => (f(a), s)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = ???

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    //input <- inputs
    //Machine(locked, candies, coins) <- inputs
  } yield ()
}


object TestState {
  import RNG._

  def main(args: Array[String]): Unit = {
    val rng = Simple(42)
    val (_, rng2) = rng.nextInt
    val (_, rng3) = rng2.nextInt

    println { "nonNegativeInt: " + nonNegativeInt(rng) }
    println { "nonNegativeInt: " + nonNegativeInt(rng2) }
    println { "nonNegativeInt: " + nonNegativeInt(rng3) }
    println { "double: " + double(rng) }
    println { "double: " + double(rng2) }
    println { "double: " + double(rng3) }

    println { "ints: " + ints(10)(rng) }

    println { "int: " + int(rng) }

    println { "double2: " + double2(rng) }
    println { "double2: " + double2(rng2) }
    println { "double2: " + double2(rng3) }

    println { "both: " + both(int, double)(rng) }

    println { "sequence: " + sequence(List(int, int))(rng) }

    println { "ints2: " + ints2(10)(rng) }

    println { "nonNegativeLessThan: " + nonNegativeLessThan(10)(rng) }
    println { "nonNegativeLessThanOrig: " + nonNegativeLessThanOrig(10)(rng) }

    def rollDie: Rand[Int] = nonNegativeLessThan(6)
    println { "rollDie: " + rollDie(Simple(1))._1 }
    println { "rollDie: " + rollDie(rollDie(Simple(5))._2)._1 }

  }
}