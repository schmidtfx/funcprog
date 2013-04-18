package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect") {
    new TestSets {
      val s = intersect((x) => (x >= 1 && x <= 10), (x) => (x <= 5))
      assert(contains(s, 1), "Intersect 1")
      assert(contains(s, 3), "Intersect 2")
      assert(contains(s, 5), "Intersect 3")
      assert(!contains(s, 6), "Intersect 4")
    }
  }

  test("diff") {
    new TestSets {
      val s = diff((x => x >= 1 && x <= 10), (x) => (x <= 5))
      assert(!contains(s, 1), "Diff 1")
      assert(!contains(s, 3), "Diff 2")
      assert(!contains(s, 5), "Diff 3")
      assert(contains(s, 6), "Diff 4")
      assert(contains(s, 7), "Diff 5")
      assert(!contains(s, 11), "Diff 6")
    }
  }

  test("filter") {
    new TestSets {
      val s = filter((x) => x >= 1 && x <= 10, (x) => (x <= 5))
      assert(contains(s, 1), "Filter 1")
      assert(contains(s, 3), "Filter 2")
      assert(contains(s, 5), "Filter 3")
      assert(!contains(s, 6), "Diff 4")
      assert(!contains(s, 7), "Diff 5")
      assert(!contains(s, 11), "Diff 6")
    }
  }

  test("forall") {
    new TestSets {
      val s = (x: Int) => x >= 1 && x <= 10
      assert(forall(s, (x => x <= 11)), "Forall 1")
      assert(!forall(s, (x => x <= 11 && x > 2)), "Forall 2")
    }
  }

  test("exists") {
    new TestSets {
      val s = (x: Int) => x >= 1 && x <= 10
      assert(exists(s, x => x == 2), "Exists 1")
      assert(exists(s, x => x != 2), "Exists 2")
      assert(!exists(s, x => x == -2), "Exists 3")
      assert(exists(s, x => x > -2), "Exists 4")
    }
  }

  test("map") {
    new TestSets {
      val s = (x: Int) => x >= 1 && x <= 10
      val t = map(s, x => x + 3)
      assert(!contains(t, 1), "Map 1")
      assert(!contains(t, 2), "Map 2")
      assert(!contains(t, 3), "Map 3")
      assert(contains(t, 4), "Map 4")
      assert(contains(t, 10), "Map 5")
      assert(contains(t, 11), "Map 6")
      assert(contains(t, 12), "Map 7")
      assert(contains(t, 13), "Map 8")
      assert(!contains(t, 14), "Map 9")
    }
  }

  test("inclusion") {
    new TestSets {
      val s = (x: Int) => x < 2
      val t1 = (x: Int) => x < 7
      val t2 = (x: Int) => x < 0

      assert(inclusion(s, t1), "Inclusion 1")
      assert(!inclusion(s, t2), "Inclusion 2")
    }
  }

  test("Equals") {
    new TestSets {
      val s1_t = (x: Int) => x < 2
      val t1 = (x: Int) => x < 2
      val t2 = (x: Int) => x < 0
      val s2_t = singletonSet(2)
      val t3 = singletonSet(2)
      val t4 = singletonSet(5)
      assert(setEquals(s1_t, t1), "Equals 1")
      assert(!setEquals(s1_t, t2), "Equals 2")
      assert(setEquals(s2_t, t3), "Equals 3")
      assert(!setEquals(s2_t, t4), "Equals 4")
    }
  }

  test("Cartesian") {
    new TestSets {
      val s = singletonSet(2)
      val t = singletonSet(5)

      val c = cartesianProduct(s, t)
      printSet(c)
    }
  }

  test("IsRelation") {
    new TestSets {
      val s = (x: Int) => x >= 2 && x <= 7
      val t1 = (x: Int) => x >= 2 && x <= 7
      val t2 = (x: Int) => x >= 9 && x <= 10
      val t3 = (x: Int) => x >= 20 && x <= 70
      val c1 = cartesianProduct(s, t1)
      val c2 = cartesianProduct(s, t2)

      assert(isRelation(c1, s, t1), "IsRelation 1")
      assert(!isRelation(c1, s, t2), "IsRelation 2")
      assert(isRelation(c2, s, t2), "IsRelation 3")
      assert(!isRelation(c1, s, t3), "IsRelation 4")
    }
  }

  test("IsFunction") {
    new TestSets {
      val f1 = (x: Int, y: Int) => y == x * x
      val s = (x: Int) => x >= 2 && x <= 7
      val t1 = (x: Int) => x >= 2 && x <= 7
      val c1 = cartesianProduct(s, t1)
      assert(f1(2, 4), "IsFunction 1")
      assert(f1(-2, 4), "IsFunction 2")
      assert(f1(0, 0), "IsFunction 3")
      assert(isRelation(f1, (x) => x >= -2 && x <= 2, (x) => x >= 0 && x <= 4), "IsFunction 4 (func: Relation Test)")
      assert(isFunction(f1, (x) => x >= -2 && x <= 2, (x) => x >= 0 && x <= 4), "IsFunction 5 (func: Function Test)")
      assert(isRelation(c1, s, t1), "IsFunction 6 (rel: Relation Test)")
      assert(!isFunction(c1, s, t1), "IsFunction 7 (rel: Function Test)")
    }
  }

  test("Post1") {
    val set1 = math.abs(_: Int) <= 4
    val set2 = math.abs(_: Int) <= 6
    val myRelation1 = math.abs(_: Int) <= 2 && math.abs(_: Int) <= 1

    val myFunction = (x: Int, y: Int) => (set1(x)) && (set2(x)) && (x == 2 * y)

    val myRelation2 = math.abs(_: Int) <= 5 && math.abs(_: Int) <= 1

    assert(isRelation(myRelation1, set1, set2))
    assert(isRelation(myRelation2, set1, set2))
    assert(isRelation(myFunction, set1, set1))
    assert(isFunction(myFunction, set1, set2))
  }
}
