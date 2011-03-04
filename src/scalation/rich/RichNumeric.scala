package scalation.rich

class RichNumeric[N: Numeric](elem: N) extends scalation.ScalaTion {

	private val evidence = implicitly[Numeric[N]]
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * The Pi function for real numbers n no less than 0
	 */
	private def gaussPi(n: Double): Double = 
	{
		import scala.math._
		// finds value to 2 decimal places
		∏(1, 30000, (k: Double) => (((k + 1) / k) ↑ n) * (k / (n + k)))
    }
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Returns the factorial
	 */
	def !(): N = 
	{
		if (elem == evidence.toInt(elem)) {
			// treat elem as integral type
			∏(1 to evidence.toInt(elem)).asInstanceOf[N]
		} else {
			// treat elem as fractional type
			gaussPi(evidence.toDouble(elem)).asInstanceOf[N]
		}
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Returns the value of the number raised to the p power as a Double. Using
	 * Knuth's up-arrow notation
	 */
	def ↑[A: Numeric](p: A): Double =
	{
		import scala.math._
		val x = evidence.toDouble(elem)
		val y = implicitly[Numeric[A]].toDouble(p)
		pow(x, y)
	}
	
	def ↑(n: Int, b: Int): Double = 
	{
		(n, b) match {
			case (1, _) => elem ↑ b
			case (_, 0) => 1
			case (_, _)	=> (elem ↑ (n-1)) ↑ (elem ↑ (n, b-1))
		}
	}
	
}