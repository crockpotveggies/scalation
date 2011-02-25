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
	 */
	def !(): A =
    {
		// cast to Int is ok because type A is implicitly Integral
		val series = 1 to evidence.toInt(elem)
    	evidence fromInt ∏(series)
    }
    
	
}