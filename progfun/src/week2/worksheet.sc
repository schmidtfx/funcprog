object worksheet {
	def sum(f: Int => Int)(a: Int, b: Int): Int = {
		def loop(a: Int, acc: Int): Int = {
			if(a > b) acc
			else loop(a + 1, acc + f(a))
		}
		loop(a, 0)
	}                                         //> sum: (f: Int => Int)(a: Int, b: Int)Int
	
	sum(x => x * x)(1,2)                      //> res0: Int = 5
	
	
	def product(f: Int => Int)(a: Int, b: Int): Int = {
		if(a > b) 1
		else f(a) * product(f)(a + 1, b)
	}                                         //> product: (f: Int => Int)(a: Int, b: Int)Int
	
	product(x => x * x)(3, 4)                 //> res1: Int = 144
	
	def fact(n: Int): Int = product(x => x)(1, n)
                                                  //> fact: (n: Int)Int
	fact(5)                                   //> res2: Int = 120
	
	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
		if(a > b) zero
		else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
	}                                         //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
	
	def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a,b)
                                                  //> product2: (f: Int => Int)(a: Int, b: Int)Int
  product2(x => x * x)(3, 4)                      //> res3: Int = 144
}