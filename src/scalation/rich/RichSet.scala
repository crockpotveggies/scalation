package scalation.rich

import scala.collection.mutable.Set

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Defines some rich methods (mostly defined with Unicode) that can be used 
 * by a Set.
 * @author Michael Cotterell
 */
class RichSet[A](set: Set[A]) 
{
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Computes the intersection between this set and another set.
     */
    def ∩(that: Set[A]): Set[A] = set & that
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     *Computes the union between this set and another set.
     */
    def ∪(that: Set[A]): Set[A] = set | that
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether a predicate holds for all elements of this set.
     */
    def ∀(p: (A) ⇒ Boolean): Boolean = set forall p
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether a predicate holds for some of the elements of this set.
     */
    def ∃(p: (A) ⇒ Boolean): Boolean = set exists p 
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether a predicate does not hold for some of the elements of 
     * this set.
     */
    def ∄(p: (A) ⇒ Boolean): Boolean = !(set exists p)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this set is a subset of another set.
     */
    def ⊂(that: Set[A]): Boolean = (set subsetOf that) && (set != that)
    def ⊆(that: Set[A]): Boolean = set subsetOf that
    def sub(that: Set[A]): Boolean = set subsetOf that
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this set is not a subset of another set.
     */
    def ⊄(that: Set[A]): Boolean = !(set subsetOf that) && (set != that) 
    def ⊈(that: Set[A]): Boolean = !(set subsetOf that)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this set is a superset of another set.
     */
    def ⊃(that: Set[A]): Boolean = (that subsetOf set) && (set != that)
    def ⊇(that: Set[A]): Boolean = that subsetOf set
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this set is not a superset of another set.
     */
    def ⊅(that: Set[A]): Boolean = !(that subsetOf set) && (set != that)
    def ⊉(that: Set[A]): Boolean = !(that subsetOf set)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this set contains an element.
     */
    def ∋(that: A): Boolean = set contains that
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this set does not contain an element.
     */
    def ∌(that: A): Boolean = !(set contains that)
    
}
