package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = {
    def loop(t: Tree[A], acc: Int): Int =  t match {
      case Leaf(v) => acc
      case Branch(l, r) => loop(l, acc + 1) max loop(r, acc + 1)
    }
    loop(t, 1)
  }

  def depth2[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => (1 + depth2(l)) max (1 + depth2(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)
  def maximum2(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)
  def depth3[A](t: Tree[A]): Int = fold(t)(x => 1)((x, y) => (x + 1) max (y + 1))
  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(x => Leaf(f(x)))(Branch(_, _))

}

object TestTree {
  import Tree._

  def main(args: Array[String]): Unit = {

    val tree = Branch(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c"))), Leaf("d"))
    val intTree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))
    val intTree2 = Branch(Branch(Leaf(1), Branch(Leaf(5), Leaf(3))), Leaf(4))
    val intTree3 = Branch(Leaf(2), Branch(Leaf(9), Leaf(4)))

    println("size: " + size(tree))
    println("size: " + size(intTree3))
    println("size2: " + size2(tree))
    println("size2: " + size2(intTree3))

    println("maximum: " + maximum(intTree))
    println("maximum: " + maximum(intTree2))
    println("maximum: " + maximum(intTree3))

    println("maximum2: " + maximum2(intTree))
    println("maximum2: " + maximum2(intTree2))
    println("maximum2: " + maximum2(intTree3))

    println("depth: " + depth(intTree))
    println("depth: " + depth(intTree2))
    println("depth: " + depth(intTree3))
    println("depth: " + depth(Leaf(1)))
    println("depth: " + depth(Branch(Leaf(1), Leaf(2))))

    println("depth2: " + depth2(intTree))
    println("depth2: " + depth2(intTree2))
    println("depth2: " + depth2(intTree3))
    println("depth2: " + depth2(Leaf(1)))
    println("depth2: " + depth2(Branch(Leaf(1), Leaf(2))))

    println("depth3: " + depth3(intTree))
    println("depth3: " + depth3(intTree2))
    println("depth3: " + depth3(intTree3))
    println("depth3: " + depth3(Leaf(1)))
    println("depth3: " + depth3(Branch(Leaf(1), Leaf(2))))

    println("map: " + map(tree)(_ + "_x"))
    println("map: " + map(intTree)(_ + 1))
    println("map: " + map(intTree2)(_ + 1))

    println("map2: " + map2(tree)(_ + "_x"))
    println("map2: " + map2(intTree)(_ + 1))
    println("map2: " + map2(intTree2)(_ + 1))
  }


}
