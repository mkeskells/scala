package scala.collection.immutable

import java.util.Collections

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AllocationTest
import scala.util.Random

@RunWith(classOf[JUnit4])
class TreeMapTest extends AllocationTest {

  @Test
  def hasCorrectDropAndTakeMethods() {
    val tree = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")

    assertEquals(TreeMap.empty[Int, String], tree take Int.MinValue)
    assertEquals(TreeMap.empty[Int, String], tree takeRight Int.MinValue)
    assertEquals(tree, tree drop Int.MinValue)
    assertEquals(tree, tree dropRight Int.MinValue)
  }
  @Test def entriesEqualSimple: Unit = {
    val tree1 = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")
    val tree2 = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(tree1, tree2)
  }
  @Test def entriesEqual: Unit = {
    val b1 = TreeMap.newBuilder[Int, String]
    for (i <- 10 to 1000) {
      b1 += i -> s"$i value"
    }
    val tree1 = b1.result()
    val b2    = TreeMap.newBuilder[Int, String]
    for (i <- 1 to 1000) {
      b2 += i -> s"$i value"
    }
    val tree2 = b2.result().drop(9)

    assertEquals(tree1, tree2)
    assertNotEquals(tree1, (tree2 + (9999 -> "zzz")))
    assertNotEquals((tree1 + (9999 -> "zzz")), (tree2))
    assertEquals((tree1 + (9999 -> "zzz")), (tree2 + (9999 -> "zzz")))
    assertNotEquals((tree1 + (9999 -> "zzz")), (tree2 + (9999999 -> "zzz")))
  }
  @Test def equalFastPath: Unit = {
    class V(val s: String) {
      override def equals(obj: Any): Boolean = obj match {
        case v: V => v.s == s
      }
    }
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
    val b1 = TreeMap.newBuilder[K, V]
    for (i <- 10 to 1000) {
      b1 += new K(i.toString) -> new V(s"$i value")
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

    exp += new K("XXX") -> new V("YYY")
    act += new K("XXX") -> new V("YYY")

    compareCount = 0
    assertEquals(exp, act)
    assertTrue(compareCount.toString, compareCount < 30)

    onlyAllocates(408)(assertEquals(exp, act))
  }
  @Test
  def plusWithContains() {
    val data = Array.tabulate(1000)(i => s"${i}Key" -> s"${i}Value")
    val tree = (TreeMap.newBuilder[String, String] ++= data).result

    data foreach {
      case (k, v) =>
        assertSame(tree, nonAllocating(tree.updated(k, v)))
    }
  }
  @Test def consistentEquals: Unit = {
    class V(val s: String) {

      override def equals(obj: Any): Boolean = obj match {
        case v: V => v.s == s
        case _    => false
      }
      override def toString: String = s"V-$s"
    }
    class K(val s: String) extends Ordered[K] {
      override def toString: String = s"K-$s"

      override def compare(that: K): Int = {
        fail("compare should not be called  - should be handled by the custom ordering")
        0
      }
      override def equals(obj: Any): Boolean = obj match {
        case k: K => k.s == s
        case _    => false
      }
      override def hashCode(): Int = s.hashCode

    }
    class CustomOrder(val selfEqual: Boolean) extends Ordering[K] {
      override def compare(x: K, y: K): Int = x.s compareTo y.s

      override def equals(obj: Any): Boolean = obj match {
        case c: CustomOrder => (c eq this) || this.selfEqual && c.selfEqual
        case _              => false
      }
    }
    val o1   = new CustomOrder(true)
    val o2_1 = new CustomOrder(false)
    val o2_2 = new CustomOrder(false)

    val b1_1 = TreeMap.newBuilder[K, V](o1)
    val b1_2 = TreeMap.newBuilder[K, V](o1)

    val b2_1 = TreeMap.newBuilder[K, V](o2_1)
    val b2_2 = TreeMap.newBuilder[K, V](o2_2)

    val bHash = HashMap.newBuilder[K, V]
    for (i <- 10 to 20) {
      b1_1 += new K(i.toString) -> new V(s"$i value")
      b1_2 += new K(i.toString) -> new V(s"$i value")

      b2_1 += new K(i.toString) -> new V(s"$i value")
      b2_2 += new K(i.toString) -> new V(s"$i value")

      bHash += new K(i.toString) -> new V(s"$i value")
    }
    val tree1_1 = b1_1.result()
    val tree1_2 = b1_2.result()

    val tree2_1 = b1_1.result()
    val tree2_2 = b1_2.result()

    val treeHash = bHash.result()

    val all = List((tree1_1, "tree1_1"), (tree1_2, "tree1_2"), (tree2_1, "tree2_1"), (tree2_2, "tree2_2"), (treeHash, "treeHash"))
    for ((lhs, lText) <- all;
         (rhs, rText) <- all) {
      assertEquals(s"$lText $rText", lhs, rhs)
      assertEquals(s"$rText $lText", rhs, lhs)
    }
  }

  @Test def keyOverwriteIn212(): Unit = {
    // see https://github.com/scala/scala/pull/7481 and https://github.com/scala/scala/pull/8783
    // for 2.13.x changes that we don't want to include in the RedBlackTree backports to 2.12.x
    // for compatibility.
    val map  = collection.immutable.TreeMap.apply(2 -> 2)(Ordering.by(x => x / 2))
    val map2 = map.updated(3, 3)
    assertEquals(List(3 -> 3), map2.toList)
  }

  @Test
  def retainLeft(): Unit = {
    case class C(a: Int)(override val toString: String)
    implicit val ordering: Ordering[C] = Ordering.by(_.a)
    val c0l = C(0)("l")
    val c0r = C(0)("r")

    def assertIdenticalKeys(expected: Map[C, Unit], actual: Map[C, Unit]): Unit = {
      val expected1, actual1 = Collections.newSetFromMap[C](new java.util.IdentityHashMap())
      expected.keys.foreach(expected1.add)
      actual.keys.foreach(actual1.add)
      assertEquals(expected1, actual1)
    }

    assertIdenticalKeys(Map((c0l, ())), HashMap((c0l, ())).++(HashMap((c0r, ()))))

    assertIdenticalKeys(Map((c0l, ())), TreeMap((c0l, ())).++(HashMap((c0r, ()))))
    assertIdenticalKeys(Map((c0l, ())), TreeMap((c0l, ())).++(TreeMap((c0r, ()))))

    assertIdenticalKeys(Map((c0l, ())), HashMap.newBuilder[C, Unit].++=(HashMap((c0l, ()))).++=(HashMap((c0r, ()))).result())

    assertIdenticalKeys(Map((c0l, ())), TreeMap.newBuilder[C, Unit].++=(TreeMap((c0l, ()))).++=(HashMap((c0r, ()))).result())
    assertIdenticalKeys(Map((c0l, ())), TreeMap.newBuilder[C, Unit].++=(TreeMap((c0l, ()))).++=(TreeMap((c0r, ()))).result())
  }

  @Test
  def overwriteEntryRegression(): Unit = {
    val x  = TreeMap(1 -> "herring", 2 -> "cod", 3 -> "salmon")
    val y  = TreeMap(3 -> "wish")
    val r1 = x ++ y
    val r2 = (x.toSeq ++ y.toSeq).toMap
    assertEquals(r1, r2)
  }

  @Test def caseIndependent1: Unit = {
    val m = scala.collection.immutable.TreeMap[String, String]()(_ compareToIgnoreCase _)
    val r = m ++ Seq("a" -> "1", "A" -> "2")
    // Note - in 2.13 this should be
    // assertEquals(Map("a" -> "2"), r)
    // as keys are retained
    assertEquals(Map("A" -> "2"), r)

  }
  @Test def caseIndependent2: Unit = {
    val m = scala.collection.immutable.TreeMap[String, String]()(_ compareToIgnoreCase _)
    val r = Seq("a" -> "1", "A" -> "2").foldLeft(m) {
      case (acc, t) => acc + t
    }
    // Note - in 2.13 this should be
    // assertEquals(Map("a" -> "2"), r)
    // as keys are retained
    assertEquals(Map("A" -> "2"), r)

  }
  @Test def unionSpecialCases: Unit = {
    val empty                   = TreeMap.empty[Int, String]
    val t: TreeMap[Int, String] = TreeMap(1 -> "", 2 -> "", 4 -> "", 6 -> "", 8 -> "", 10 -> "")
    assertSame(t, t ++ empty)
    assertSame(t, empty ++ t)
    assertSame(t, t ++ t)

    nonAllocating(t ++ empty)
    nonAllocating(empty ++ t)
    nonAllocating(t ++ t)
  }

  def unionSubset(size: Int, elementToRemove: Int): Unit = {
    val superset = TreeMap[Int, String](Array.tabulate(size) { x => x -> "" }: _*)
    val subset   = superset - elementToRemove

    //its a bit sad that this allocates at all

    onlyAllocates(500)(superset ++ subset)
  }


  @Test def unionWithSubset0: Unit = {
    unionSubset(100, 0)
  }
  @Test def unionWithSubset1: Unit = {
    unionSubset(100, 1)
  }
  @Test def unionWithSubset2: Unit = {
    unionSubset(100, 5)
  }
  @Test def unionWithSubset3: Unit = {
    unionSubset(100, 60)
  }
  @Test def unionWithSubset4: Unit = {
    unionSubset(100, 99)
  }

  private def validateMap[A, B](original: TreeMap[A, B], result: TreeMap[A, B])(implicit ordering: Ordering[A]): TreeMap[A, B] = {
    NewRedBlackTree.validate(original.tree0, result.tree0)
    NewRedBlackTree.validate2(original.tree0, result.tree0)
    result
  }
  private def validateSet[A](original: TreeSet[A], result: TreeSet[A])(implicit ordering: Ordering[A]): TreeSet[A] = {
    NewRedBlackTree.validate(original.tree, result.tree)
    NewRedBlackTree.validate2(original.tree, result.tree)
    result
  }
  @Test
  def randomTreePlusMinus() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))
    }
  }
  @Test
  def randomTreeBuild() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      val builder = TreeMap.newBuilder[Int, String]
      builder ++= r.shuffle(m.toList)
      val res = validateMap(m, builder.result())

      assertEquals(m, res)
    }
  }
  @Test
  def randomTreeSplit() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      val (m1, m2) = m.splitAt(r.nextInt(1000))

      validateMap(m, m1)
      validateMap(m, m2)
    }
  }
  @Test
  def randomTreeDrop() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.drop(r.nextInt(1000)))
    }
  }
  @Test
  def randomTreeTake() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.take(r.nextInt(1000)))
    }
  }
  @Test
  def randomTreeSlice() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.slice(r.nextInt(1000), r.nextInt(1000)))
    }
  }
  @Test
  def randomTreeInit() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.init)
    }
  }
  @Test
  def randomTreeTail() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.tail)
    }
  }
  @Test
  def randomTreeFrom() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.from(r.nextInt(1000)))
    }
  }
  @Test
  def randomTreeTo() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.to(r.nextInt(1000)))
    }
  }
  @Test
  def randomTreeUntil() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.until(r.nextInt(1000)))
    }
  }
  @Test
  def randomTreeRange() {
    val r = new Random(0)
    var m = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m = validateMap(m, m + ((r.nextInt(1000), "")))
      m = validateMap(m, m - r.nextInt(1000))

      validateMap(m, m.range(r.nextInt(1000), r.nextInt(1000)))
    }
  }

  def checkUnion(union: TreeMap[Int, String], m1: TreeMap[Int, String], m2: TreeMap[Int, String], v1: String, v2: String): Unit = {
    union.foreachEntry { (k, v) =>
      m2.get(k) match {
        case Some(m2v) =>
          if (v2 ne null)
            assertEquals(v2, m2v)
          assertEquals(m2v, v)
        case None =>
          assertEquals(Some(v1), m1.get(k))
          assertEquals(v1, v)
      }
    }
  }

  @Test
  def randomTreeUnion() {
    val r  = new Random(0)
    var m1 = TreeMap[Int, String]()
    var m2 = TreeMap[Int, String]()
    for (i <- 1 to 10000) {
      m1 = validateMap(m1, m1 + ((r.nextInt(1000), "1")))
      m1 = validateMap(m1, m1 - r.nextInt(1000))

      m2 = validateMap(m2, m2 + ((r.nextInt(1000), "2")))
      m2 = validateMap(m2, m2 - r.nextInt(1000))

      var union = validateMap(m1, m1 ++ m2)
      checkUnion(union, m1, m2, "1", "2")

      var m3 = m1
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      //subset
      union = validateMap(m1, m1 ++ m3)
      checkUnion(union, m1, m3, "1", null)

      m3 = m1
      m3 = validateMap(m3, m3 + ((r.nextInt(1000), "3")))
      m3 = validateMap(m3, m3 + ((r.nextInt(1000), "3")))
      m3 = validateMap(m3, m3 + ((r.nextInt(1000), "3")))
      //superset
      union = validateMap(m1, m1 ++ m3)
      checkUnion(union, m1, m3, "1", null)

      m3 = validateMap(m3, m3 - r.nextInt(1000))
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      //overlap
      union = validateMap(m1, m1 ++ m3)
      checkUnion(union, m1, m3, "1", null)
    }
  }
  @Test
  def randomTreeDiff() {
    val r  = new Random(0)
    var m1 = TreeMap[Int, String]()
    var m2 = TreeSet[Int]()
    for (i <- 1 to 10000) {
      m1 = validateMap(m1, m1 + ((r.nextInt(1000), "")))
      m1 = validateMap(m1, m1 - r.nextInt(1000))

      m2 = validateSet(m2, m2 + r.nextInt(1000))
      m2 = validateSet(m2, m2 - r.nextInt(1000))

      validateMap(m1, m1 -- m2)

      var m3 = m1.keySet.asInstanceOf[TreeSet[Int]]
      m3 = validateSet(m3, m3 - r.nextInt(1000))
      m3 = validateSet(m3, m3 - r.nextInt(1000))
      m3 = validateSet(m3, m3 - r.nextInt(1000))
      //subset
      validateMap(m1, m1 -- m2)

      m3 = m1.keySet.asInstanceOf[TreeSet[Int]]
      m3 = validateSet(m3, m3 + r.nextInt(1000))
      m3 = validateSet(m3, m3 + r.nextInt(1000))
      m3 = validateSet(m3, m3 + r.nextInt(1000))
      //superset
      validateMap(m1, m1 -- m2)

      m3 = validateSet(m3, m3 - r.nextInt(1000))
      m3 = validateSet(m3, m3 - r.nextInt(1000))
      m3 = validateSet(m3, m3 - r.nextInt(1000))
      //overlap
      validateMap(m1, m1 -- m2)
    }
  }
  @Test
  def randomTreeFilter() {
    val r  = new Random(0)
    var m1 = TreeMap[Int, String]()
    var m2 = HashSet[Int]()
    for (i <- 1 to 100000) {
      m1 = validateMap(m1, m1 + ((r.nextInt(1000), "")))
      m1 = validateMap(m1, m1 - r.nextInt(1000))

      m2 = m2 + r.nextInt(1000)
      m2 = m2 - r.nextInt(1000)

      //we want this as a function, avoid any special optimised code
      validateMap(m1, m1.filter(x => m2 contains x._1))

      var m3 = m1
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      //subset
      validateMap(m1, m1.filter(x => m3 contains x._1))

      m3 = m1
      m3 = validateMap(m3, m3 + ((r.nextInt(1000), "")))
      m3 = validateMap(m3, m3 + ((r.nextInt(1000), "")))
      m3 = validateMap(m3, m3 + ((r.nextInt(1000), "")))
      //superset
      validateMap(m1, m1.filter(x => m3 contains x._1))

      m3 = validateMap(m3, m3 - r.nextInt(1000))
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      m3 = validateMap(m3, m3 - r.nextInt(1000))
      //overlap
      validateMap(m1, m1.filter(x => m3 contains x._1))
    }
  }
  @Test
  def randomTreeTransform() {
    val r  = new Random(0)
    var m1 = TreeMap[Int, String]()
    for (i <- 1 to 100000) {
      m1 = validateMap(m1, m1 + ((r.nextInt(1000), "")))
      m1 = validateMap(m1, m1 - r.nextInt(1000))

      validateMap(m1, m1.transform((k, v) => if (r.nextBoolean()) "" else "z"))
    }
  }
  @Test
  def randomTreePartition() {
    val r  = new Random(0)
    var m1 = TreeMap[Int, String]()
    var m2 = TreeSet[Int]()
    for (i <- 1 to 100000) {
      m1 = validateMap(m1, m1 + ((r.nextInt(1000), "")))
      m1 = validateMap(m1, m1 - r.nextInt(1000))

      m2 = m2 + r.nextInt(1000)
      m2 = m2 - r.nextInt(1000)

      //we want this as a function, avoid any special optimised code
      {
        val (p1, p2) = m1.partition(x => m2 contains x._1)
        validateMap(m1, p1)
        validateMap(m1, p2)
      }

      var m3 = m1
      m3 = m3 - r.nextInt(1000)
      m3 = m3 - r.nextInt(1000)
      m3 = m3 - r.nextInt(1000)

      //subset
      {
        val (p1, p2) = m1.partition(x => m3 contains x._1)
        validateMap(m1, p1)
        validateMap(m1, p2)
      }

      m3 = m1
      m3 = m3 + ((r.nextInt(1000), ""))
      m3 = m3 + ((r.nextInt(1000), ""))
      m3 = m3 + ((r.nextInt(1000), ""))

      //superset
      {
        val (p1, p2) = m1.partition(x => m3 contains x._1)
        validateMap(m1, p1)
        validateMap(m1, p2)
      }

      m3 = m3 - r.nextInt(1000)
      m3 = m3 - r.nextInt(1000)
      m3 = m3 - r.nextInt(1000)

      //overlap
      {
        val (p1, p2) = m1.partition(x => m3 contains x._1)
        validateMap(m1, p1)
        validateMap(m1, p2)
      }
    }
  }

}
