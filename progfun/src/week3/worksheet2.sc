package week3

import week3._

object worksheet2 {
  new Rational(2, 4)                              //> res0: week3.Rational = 1/2
  
  def error(msg: String) = throw new Error(msg)   //> error: (msg: String)Nothing
  val x = null                                    //> x  : Null = null
  val y: String = null                            //> y  : String = null
  
  if (true) 1 else false                          //> res1: AnyVal = 1
}

trait Planar {
	def height: Int
	def width: Int
	def surface = height * width
}