package scala.collection.immutable

import java.util
import java.util.Collections

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class TreeSetTest extends AllocationTest{

  @Test
  def hasCorrectDropAndTakeMethods() {
    val set = TreeSet(1, 2, 3)

    assertEquals(TreeSet.empty[Int], set take Int.MinValue)
    assertEquals(TreeSet.empty[Int], set takeRight Int.MinValue)
    assertEquals(set, set drop Int.MinValue)
    assertEquals(set, set dropRight Int.MinValue)
  }

  @Test
  def min(): Unit = {
    assertEquals(1, TreeSet(1, 2, 3).min)
    assertEquals(3, TreeSet(1, 2, 3).min(implicitly[Ordering[Int]].reverse))

    try {
      TreeSet.empty[Int].min
      fail("expect UnsupportedOperationException")
    } catch {
      case e: UnsupportedOperationException =>
        assertEquals("empty.min", e.getMessage)
    }
  }

  @Test
  def max(): Unit = {
    assertEquals(3, TreeSet(1, 2, 3).max)
    assertEquals(1, TreeSet(1, 2, 3).max(implicitly[Ordering[Int]].reverse))

    try {
      TreeSet.empty[Int].max
      fail("expect UnsupportedOperationException")
    } catch {
      case e: UnsupportedOperationException =>
        assertEquals("empty.max", e.getMessage)
    }
  }


  @Test def keysEqualSimple: Unit = {
    val tree = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(Set(1,2,3), tree.keySet)
    val tree2 = TreeMap(1 -> "x", 2 -> "y", 3 -> "z")
    assertEquals(tree2.keySet, tree.keySet)
    val tree3 = TreeSet(1,2,3)
    assertEquals(tree3, tree.keySet)
    assertEquals(tree.keySet, tree3)
  }
  @Test def valuesEqual: Unit = {
    val data = (Array.tabulate(1000) { i => i.toString}).sorted
    val expectedData = data.drop(30)

    Collections.shuffle(util.Arrays.asList(data))

    val tree1 = (TreeSet.newBuilder[String] ++= expectedData).result()
    val tree2 = (TreeSet.newBuilder[String] ++= data).result().drop(30)

    assertEquals(tree1, tree1)
    assertEquals(tree1, tree2)
  }
  @Test def keysFromMapEqualFastPath: Unit = {
    class V(s: String) {

      override def equals(obj: Any): Boolean = {
        fail("equals should not be called = the trees should be  the same")
        false
      }
    }
    val b1 = TreeMap.newBuilder[String, V]
    for ( i <- 10 to 1000) {
      b1 += s"$i" -> new V(s"$i value")
    }
    val tree1 = b1.result()

    assertEquals(tree1.drop(5).keySet, tree1.drop(5).keySet)
  }
  @Test def equalFastPath: Unit = {
    var compareCount = 0
    class K(val s: String) extends Ordered[K] {
      override def toString: String = s"K-$s"

      override def compare(that: K): Int = {
        val res = s.compareTo(that.s)
        compareCount += 1
        res
      }

      override def equals(obj: Any): Boolean = {
        fail("equals should not be called = the trees should be ordered and compared via the sort order")
        false
      }
    }
    val b1 = TreeSet.newBuilder[K]
    for ( i <- 10 to 1000) {
      b1 += new K(i.toString)
    }
    val tree1 = b1.result()
    compareCount = 0
    nonAllocating(assertEquals(tree1, tree1))
    assertEquals(0, compareCount)

    var exp = tree1.drop(5)
    var act = tree1.drop(5)
    compareCount = 0
    onlyAllocates(240)(assertEquals(exp, act))
    assertEquals(0, compareCount)

    exp += new K("XXX")
    act += new K("XXX")

    compareCount = 0
    assertEquals(exp, act)
    assertTrue(compareCount.toString, compareCount < 30)
    //we cant combine this with th above assertion as onlyAllocates run lots of time to determne the allocation
    onlyAllocates(408)(assertEquals(exp, act))
  }

  @Test
  def plusWithContains() {
    val data = Array.tabulate(1000)(i => s"${i}Value")
    val tree = (TreeSet.newBuilder[String] ++= data).result

    data foreach {
      case (k) =>
        assertSame(tree, nonAllocating(tree + k))
    }
  }
  @Test
  def plusWithContainsFromMap() {
    val data = Array.tabulate(1000)(i => s"${i}Key" -> s"${i}Value")
    val tree = (TreeMap.newBuilder[String, String] ++= data).result.keySet

    data foreach {
      case (k, v) =>
        assertSame(tree, nonAllocating(tree + k))
    }
  }
}
