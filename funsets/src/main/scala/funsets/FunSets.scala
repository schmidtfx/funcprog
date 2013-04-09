package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean
  type Set2 = (Int, Int) => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = x => x == elem 
  
  def boundedSet(a: Int, b: Int): Set = x => x >= a && x <= b
  
  def arbitrarySet(a: Int*): Set = x => a.contains(x)

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x) => contains(s, x) || contains(t, x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set = x => contains(s, x) && contains(t, x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = x => contains(s, x) && !(contains(t, x))

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = x => contains(s, x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = 	!forall(s, b => !p(b))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = (x) => exists(s, (a) => f(a) == x)
  
  def reduce(s: Set, f: (Int, Int) => Int, zero: Int): Int = {
    def iter(a: Int, acc: Int): Int = {
      if(a > bound) acc
      else if (contains(s, a)) iter(a + 1, f(acc, a))
      else iter(a + 1, acc)
    }
    iter(-bound, zero)
  }
  
  def inclusion(s: Set, t: Set): Boolean = forall(s, t)
  
  def setEquals(s: Set, t: Set): Boolean = forall(s, t) && forall(t, s)
  
  def cartesianProduct(s: Set, t: Set): Set2 = (x, y) => contains(s, x) && contains(t, y)
  
  def contains(s: Set2, elem1: Int, elem2: Int) = s(elem1, elem2)
  
  def isRelation(r: Set2, s: Set, t: Set): Boolean = exists(s, (x) => exists(t, (y) => r(x, y)))
  
  def isFunction(r: Set2, s: Set, t: Set): Boolean = forall(s, (x) => !forall(t, (y) => r(x, y)))

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }
  
  def toString(s: Set2): String = {
    val xs = for(i <- -bound to bound; j <- -bound to bound if contains(s, i, j)) yield (i, j)
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
  
  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set2) {
    println(toString(s))
  }
}
