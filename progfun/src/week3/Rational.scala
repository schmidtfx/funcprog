package week3

class Rational(x: Int, y: Int) {
	require(y != 0, "dominator must be nonzero")
	val g = gcd(x, y)
	val numer = x / g
	val denom = y / g
	
	def +(that: Rational) =
		new Rational(
			this.numer * that.denom + that.numer * this.denom,
			this.denom * that.denom)
			
	def unary_- : Rational = new Rational(-this.numer, this.denom)
	
	def - (that: Rational) = this + -that
	
	def < (that: Rational) = this.numer * that.denom < that.numer * this.denom
	
	def max(that: Rational) = if(this < that) that else this
			
	override def toString = numer + "/" + denom
	
	private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
}