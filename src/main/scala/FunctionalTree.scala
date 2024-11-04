package com.latwal

sealed trait FunctionalTree[+A]

case class Leaf[A](value: A) extends FunctionalTree[A]

case class Branch[A](left: FunctionalTree[A] | Null, right: FunctionalTree[A] | Null) extends FunctionalTree[A]

object FunctionalTree {

  def size[A](tree: FunctionalTree[A] | Null): Int = {
    tree match
      case null => 0
      case Leaf(value) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
  }

  def apply[A](as: A*): FunctionalTree[A] = {
    if (as.isEmpty) Branch(null, null)
    else {
      def mergeTwoTrees(t1: FunctionalTree[A], t2: FunctionalTree[A]) = Branch(t1, t2)

      def splitAndMerge(al: Seq[A]): FunctionalTree[A] | Null = {
        val (l, r) = al.splitAt(al.size / 2)
        val rightTree = if (r.isEmpty) null
        else if (r.size == 1) Leaf(r.head)
        else splitAndMerge(r)
        val leftTree = if (l.isEmpty) null
        else if (l.size == 1) Leaf(l.head)
        else splitAndMerge(l)
        mergeTwoTrees(leftTree, rightTree)
      }

      splitAndMerge(as)
    }
  }

  def depth[A](functionalTree: FunctionalTree[A]): Int = {
    functionalTree match
      case Leaf(value) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: FunctionalTree[A])(f: A => B): FunctionalTree[B] = {
    def go(t: FunctionalTree[A]): FunctionalTree[B] = {
      t match
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(go(left),go(right))
    }
    go(tree)
  }

  def main(args: Array[String]): Unit = {
    val t1 = FunctionalTree(1, 2, 3, 4)
    println(t1)
    println(depth(t1))
    println(
      map(t1)( _ * 2)
    )
  }

}