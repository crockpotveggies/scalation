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
	
}