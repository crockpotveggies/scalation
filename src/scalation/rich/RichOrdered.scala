package scalation.rich

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Defines some rich methods (mostly defined with Unicode) that can be used 
 * by an ordered type.
 * @author Michael Cotterell
 */
class RichOrdered[A <% Ordered[A]](elem: A) {

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether that <= that
	 * @param that
	 * @return
	 */
	def ≤ (that: A): Boolean = elem <= that

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether this >= that
	 * @param that
	 * @return
	 */
	def ≥ (that: A): Boolean = elem >= that
	
}