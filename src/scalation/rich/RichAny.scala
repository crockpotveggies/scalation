/* $Id$ */

package scalation.rich

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Defines some rich methods (mostly defined with Unicode) that can be used 
 * by an element.
 * @author Michael Cotterell
 */
class RichAny[A](elem: A) extends scalation.ScalaTion
{
	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this element is contained in a set
     */
    def ∈(set: Set[A]): Boolean = set ∋ elem
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this element is not contained in a set
     */
    def ∉(set: Set[A]): Boolean = set ∌ elem
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this element is identical to that element
     */
    def ≡(that: Any): Boolean = this == that
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this element is not identical to that element
     */
    def ≢(that: Any): Boolean = !(this ≡ that)
    
}