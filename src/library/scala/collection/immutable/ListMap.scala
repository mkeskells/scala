/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import generic._
import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list map with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#list-maps "Scala's Collection Library overview"]]
  * section on `List Maps` for more information.
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  */
object ListMap extends ImmutableMapFactory[ListMap] {

  /**
    * $mapCanBuildFromInfo
    */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ListMap[A, B]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, (A, B), ListMap[A, B]]]
  private[this] val ReusableCBF = new MapCanBuildFrom[Any, Any]

  def empty[A, B]: ListMap[A, B] = EmptyListMap.asInstanceOf[ListMap[A, B]]

  @SerialVersionUID(-8256686706655863282L)
  private object EmptyListMap extends ListMap[Any, Nothing]
}

/**
  * This class implements immutable maps using a list-based data structure. List map iterators and
  * traversal methods visit key-value pairs in the order whey were first inserted.
  *
  * Entries are stored internally in reversed insertion order, which means the newest key is at the
  * head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and `init`
  * are O(1). Other operations, such as inserting or removing entries, are also O(n), which makes
  * this collection suitable only for a small number of elements.
  *
  * Instances of `ListMap` represent empty maps; they can be either created by calling the
  * constructor directly, or by applying the function `ListMap.empty`.
  *
  * @tparam A the type of the keys contained in this list map
  * @tparam B the type of the values associated with the keys
  *
  * @author Matthias Zenger
  * @author Martin Odersky
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
@SerialVersionUID(301002838095710379L)
sealed class ListMap[A, +B] extends AbstractMap[A, B]
  with Map[A, B]
  with MapLike[A, B, ListMap[A, B]]
  with Serializable
  with HasForeachEntry[A,B] {

  override def empty = ListMap.empty

  override def size: Int = 0
  override def isEmpty: Boolean = true

  def get(key: A): Option[B] = None

  private[immutable] def foreachEntry[U](f: (A, B) => U): Unit = {
    var current = this
    while (!current.isEmpty) {
      f(current.key, current.value)
      current = current.next
    }
  }

  override def hashCode(): Int = {
    if (isEmpty) {
      MurmurHash3.emptyMapHash
    } else {
      val hasher = new Map.HashCodeAccumulator()
      foreachEntry(hasher)
      hasher.finalizeHash
    }
  }

  override def updated[B1 >: B](key: A, value: B1): ListMap[A, B1] = new Node[B1](key, value)

  def +[B1 >: B](kv: (A, B1)): ListMap[A, B1] = new Node[B1](kv._1, kv._2)
  def -(key: A): ListMap[A, B] = this

  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): ListMap[A, B1] =
    if (xs.isEmpty) this
    else ((repr: ListMap[A, B1]) /: xs) (_ + _)

  def iterator: Iterator[(A, B)] = {
    def reverseList = {
      var curr: ListMap[A, B] = this
      var res: List[(A, B)] = Nil
      while (!curr.isEmpty) {
        res = (curr.key, curr.value) :: res
        curr = curr.next
      }
      res
    }
    reverseList.iterator
  }

  protected def key: A = throw new NoSuchElementException("key of empty map")
  protected def value: B = throw new NoSuchElementException("value of empty map")
  protected def next: ListMap[A, B] = throw new NoSuchElementException("next of empty map")

  override def stringPrefix = "ListMap"
  private[ListMap] def newNode[B1 >: B](k: A, v: B1): Node[B1] = new Node(k, v)

  /**
    * Represents an entry in the `ListMap`.
    */
  @SerialVersionUID(-6453056603889598734L)
  protected class Node[B1 >: B](override protected val key: A,
                                override protected val value: B1) extends ListMap[A, B1] with Serializable {
    def filterUnordered(p: ((A, B1)) => Boolean, negate: Boolean): ListMap[A, B1] = ???

    def merge0[B2 >: B1](kvs: Node[B2], merger: HashMap.Merger[A, B2]): Node[B2] = ???

    def transformInordered[W](f: (A, B) => W): Node[W] = ???


    override def size: Int = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(cur: ListMap[A, B1], acc: Int): Int =
      if (cur.isEmpty) acc
      else sizeInternal(cur.next, acc + 1)

    override def isEmpty: Boolean = false

    override def apply(k: A): B1 = applyInternal(this, k)

    @tailrec private[this] def applyInternal(cur: ListMap[A, B1], k: A): B1 =
      if (cur.isEmpty) throw new NoSuchElementException("key not found: " + k)
      else if (k == cur.key) cur.value
      else applyInternal(cur.next, k)

    override def get(k: A): Option[B1] = getInternal(this, k)

    @tailrec private[this] def getInternal(cur: ListMap[A, B1], k: A): Option[B1] =
      if (cur.isEmpty) None
      else if (k == cur.key) Some(cur.value)
      else getInternal(cur.next, k)

    override def contains(k: A): Boolean = findIndexInternal(k,this, 0) != -1

    override def updated[B2 >: B1](k: A, v: B2): ListMap[A, B2] = updatedInternal(k, v, null, null)

    /**
     * update reflecting merger semantics
     * @param merger merger to consider. If the merger is null, then strict order is applied
     * @return
     */
    private[immutable] def updatedInternal[B2 >: B1](k: A, v: B2, kvOrNull: (A, B2), merger: HashMap.Merger[A, B2]): ListMap[A, B2] = {
      val atIndex = findIndexInternal(k, this, 0)
      if (atIndex == -1) new Node[B2](k, v)
      else if (atIndex == 0 && (merger eq null) && (value.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef]))
        this.asInstanceOf[ListMap[A, B2]]
      else {
        val node = getAtIndex(atIndex, this)
        if (merger eq null)
          removeAtIndex(atIndex).newNode[B2](node.key:A, v:B2)
        else if (merger eq HashMap.defaultMerger)
          //prefer the existing
          this
        else if (merger eq HashMap.defaultMerger.invert)
          //prefer the new k/v
          if ((node.value.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef]) &&
              (node.key.asInstanceOf[AnyRef] eq k.asInstanceOf[AnyRef]))
            this
          else removeAtIndex(atIndex).newNode[B2](k, v)
        else if (merger eq HashMap.concatMerger)
          // prefer existing key and new value
          if (node.value.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef])
            this
          else removeAtIndex(atIndex).newNode[B2](node.key, v)
        else if (merger eq HashMap.concatMerger.invert)
          //prefer the new k and old value
          if (node.key.asInstanceOf[AnyRef] eq k.asInstanceOf[AnyRef])
            this
          else removeAtIndex(atIndex).newNode[B2](k, node.value)
        else {
          val kv = if (kvOrNull eq null) (k,v) else kvOrNull
          val merged = merger((node.key, node.value), kv)
          if ((merged._1.asInstanceOf[AnyRef] eq node.key.asInstanceOf[AnyRef]) &&
             (merged._2.asInstanceOf[AnyRef] eq node.value.asInstanceOf[AnyRef]))
            this
          else removeAtIndex(atIndex).newNode[B2](merged._1, merged._2)
        }
      }
    }


    override def head: (A, B1) = (key, value)
    override def tail: ListMap[A, B1] = drop(1)
    override def drop(n: Int): ListMap[A, B1] = dropInternal(this, n)
    @tailrec final def dropInternal(cur: ListMap[A, B1], n: Int): ListMap[A, B1] = {
      if (n <= 0 || cur.isEmpty) cur
      else dropInternal(cur.next, n - 1)
    }

    override def +[B2 >: B1](kv: (A, B2)): ListMap[A, B2] = updated(kv._1, kv._2)

    override def -(k: A): ListMap[A, B1] = removeAtIndex(findIndexInternal(k, this, 0))

    @tailrec private[this] def getAtIndex(index: Int, cur: ListMap[A, B1]): ListMap[A, B1] =
      if (index == 0) cur
      else getAtIndex(index -1, cur.next)

    @tailrec private[this] def findIndexInternal(k: A, cur: ListMap[A, B1], currentIndex: Int): Int =
      if (cur.isEmpty) -1
      else if (k == cur.key) currentIndex
      else findIndexInternal(k, cur.next, currentIndex + 1)

    private[this] def removeAtIndex(index: Int): ListMap[A, B1] = {
      index match {
        case -1 => this
        case 0 => next
        case 1 =>
          val n0 = this
          val n2 = next.next
          n2.newNode(n0.key, n0.value)
        case 2 =>
          val n0 = this
          val n1 = n0.next
          val n3 = n1.next.next
          val new1 = n3.newNode(n1.key, n1.value)
          val new0 = new1.newNode(n0.key, n0.value)
          new0
        case 3 =>
          val n0 = this
          val n1 = n0.next
          val n2 = n1.next
          val n4 = n2.next.next
          val new2 = n4.newNode(n2.key, n2.value)
          val new1 = new2.newNode(n1.key, n1.value)
          val new0 = new1.newNode(n0.key, n0.value)
          new0
        case 4 =>
          val n0 = this
          val n1 = n0.next
          val n2 = n1.next
          val n3 = n2.next
          val n5 = n3.next.next
          val new3 = n5.newNode(n3.key, n3.value)
          val new2 = new3.newNode(n2.key, n2.value)
          val new1 = new2.newNode(n1.key, n1.value)
          val new0 = new1.newNode(n0.key, n0.value)
          new0
        case _ =>
          val nodes = new Array[Node[B1]](index-1)
          @tailrec def fill(index:Int, cur: ListMap[A, B1]): ListMap[A, B1] =
            if (nodes.length >= index) cur.next
            else {
              nodes(index) = cur.asInstanceOf[Node[B1]]
              fill(index + 1, cur.next)
            }
          @tailrec def makeNodes(index:Int, tail: ListMap[A, B1]): ListMap[A, B1] =
            if (index < 0) tail
            else {
              val from = nodes(index)
              makeNodes(index -1, tail.newNode(from.key, from.value))
            }
          makeNodes(index -1,fill(index-1, this))
      }
    }

    override protected def next: ListMap[A, B1] = ListMap.this

    override def last: (A, B1) = (key, value)
    override def init: ListMap[A, B1] = next
  }
}
