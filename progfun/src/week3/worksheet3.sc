package week3

object worksheet3 {
	def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)week3.Cons[T]
	def get[T](n: Int, xs: List[T]): T = {
		if(xs.isEmpty) throw new IndexOutOfBoundsException()
		else if (n == 0) return xs.head
		else get(n - 1, xs.tail)
	}                                         //> get: [T](n: Int, xs: week3.List[T])T
	singleton[Int](1)                         //> res0: week3.Cons[Int] = week3.Cons@5535cbe
	singleton[Boolean](true)                  //> res1: week3.Cons[Boolean] = week3.Cons@23edc0ad
	singleton(1)                              //> res2: week3.Cons[Int] = week3.Cons@4e04b048
	singleton(true)                           //> res3: week3.Cons[Boolean] = week3.Cons@15d16efc
	
	get(0, singleton(1))                      //> res4: Int = 1
	get(1, new Cons(1, singleton(2)))         //> res5: Int = 2
}

trait List[T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty = false
}

class Nil[T] extends List[T] {
	def isEmpty = true
	def head = throw new NoSuchElementException("Nil.head")
	def tail = throw new NoSuchElementException("Nil.tail")
}