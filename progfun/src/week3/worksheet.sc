package week3

object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val t1 = new NonEmpty(3, Empty, Empty)          //> t1  : week3.NonEmpty = {.3.}
  val t2 = t1 incl 4                              //> t2  : week3.IntSet = {.3{.4.}}
  val t3 = t2 incl 1                              //> t3  : week3.IntSet = {{.1.}3{.4.}}
  
  Empty contains 1                                //> res0: Boolean = false
  
  (new NonEmpty(7, Empty, Empty)) contains 7      //> res1: Boolean = true
}

abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

object Empty extends IntSet {
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
	def contains(x: Int): Boolean = false
	override def toString() = "."
	def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def contains(x: Int): Boolean = {
		if(x < elem) left contains x
		else if(x > elem) right contains x
		else true
	}
	
	def incl(x: Int): IntSet = {
		if(x < elem) new NonEmpty(elem, left incl x, right)
		else if(x > elem) new NonEmpty(elem, left, right incl x)
		else this
	}
	
	override def toString() = "{" + left + elem + right + "}"
	
	def union(other: IntSet): IntSet = ((left union right) union other) incl elem
}

abstract class Base {
	def foo = 1
	def bar: Int
}

class Sub extends Base
{
	override def foo = 2
	def bar = 3
}