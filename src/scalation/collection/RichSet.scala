package scalation.collection

import scala.collection.mutable.Set
import scalation.collection.RichSet._

object RichSet
{
	implicit def Set2RichSet[A](set: Set[A]) = new RichSet[A](set)
}

class RichSet[A](set: Set[A]) 
{
	/**
	 * Computes the intersection between this set and another set.
	 */
	def ∩(that: Set[A]) = set & that
	
	/**
	 *Computes the union between this set and another set.
	 */
	def ∪(that: Set[A]) = set | that
	
	/**
	 * Tests whether a predicate holds for all elements of this set.
	 */
	def ∀(p: (A) ⇒ Boolean) = set forall p
	
	/**
	 * Tests whether a predicate holds for some of the elements of this set.
	 */
	def ∃(p: (A) ⇒ Boolean) = set exists p 
	
	/**
	 * Tests whether a predicate does not hold for some of the elements of this set.
	 */
	def ∄(p: (A) ⇒ Boolean) = !(set exists p)
	
	/**
	 * Returns an empty set
	 */
	def ∅ = set.empty
	
	/**
	 * Tests whether this set is a subset of another set.
	 */
	def ⊂(that: Set[A]) = set subsetOf that
	
	/**
	 * Tests whether this set is not a subset of another set.
	 */
	def ⊄(that: Set[A]) = !(set subsetOf that)
	
	/**
	 * Tests whether this set is a superset of another set.
	 */
	def ⊃(that: Set[A]) = that subsetOf set
	
	/**
	 * Tests whether this set is not a superset of another set.
	 */
	def ⊅(that: Set[A]) = that subsetOf set
	
}

object RichSetTest 
{
	
	def main(args: Array[String]): Unit = {
	
		val x = Set(1, 2, 3, 4)
		val y = Set(0, 2, 5, 4)
		
		println( x ∩ y )
		
		println( Set(1, 2) ⊂ Set(1, 2, 3) )
		println( Set(1, 2) ⊂ Set(1, 2) )
		
		println( Set(1, 2, 3) ⊃ Set(1, 2) )
		
	}
	
}


