/* $Id$ */

package scalation.rich

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Defines some rich methods (mostly defined with Unicode) that can be used 
 * by an integral type.
 * @author Michael Cotterell
 */
class RichIntegral[A: Integral](elem: A) extends scalation.ScalaTion {

	private val evidence = implicitly[Integral[A]]
	
	/**
	 * Returns the factorial
	 * @todo once we've implemented limits and definite integrals, we should
	 *       implement this in a RichNumeric class and use the Gamma function
	 */
	def !(): A =
    {
		// cast to Int is ok because type A is implicitly Integral
		val series = 1 to evidence.toInt(elem)
    	evidence fromInt ∏(series)
    }
	
	/**
	 * Returns the value of the number raised to the p power.
	 * @param p
	 * @param evidence$2
	 * @return
	 */
	def ↑[I <% Integral[I]](p: I): A =
	{
		import scala.math._
		pow(evidence.toInt(elem), p.toInt(p)).asInstanceOf[A]
	}
    
	
}