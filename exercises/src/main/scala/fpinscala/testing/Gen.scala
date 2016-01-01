package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop { ct =>
  def check: Boolean

  def &&(other: Prop) = new Prop {
    def check = ct.check && other.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen1[A](sample: State[RNG,A]) {
  def choose(start: Int, stopExclusive: Int): Gen1[Int] =
    ???
}

object Gen {
   def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

object TestGen {
  def main(args: Array[String]): Unit = {
    def a = new Prop() { override def check: Boolean = true }
    def b = new Prop() { override def check: Boolean = false }
    println(a && b check)
    println(a && a check)
  }
}

