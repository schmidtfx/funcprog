package funsets

object Worksheet {
  import FunSets._
  
  val set1 = (x: Int) => x >= 1 && x < 10         //> set1  : Int => Boolean = <function1>
  val set2 = filter(set1, (x) => x % 3 == 0 || x % 5 == 0)
                                                  //> set2  : Int => Boolean = <function1>
 
 	val sum1 = reduce(set1, (acc, x) => acc + x, 0)
                                                  //> sum1  : Int = 45
  val sum2 = reduce(set2, (acc, x) => acc + x, 0) //> sum2  : Int = 23
  
  printSet(set2)                                  //> {3,5,6,9}
}