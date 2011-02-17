package scalation.collection

import scala.collection.mutable.Set
import scalation.collection.RichSet._

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Defines the implicit conversions that make RichSet interesting.
 * @author Michael Cotterell
 */
object RichSet
{
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Defines the implicit conversion from a Set to a RichSet
	 */
	implicit def Set2RichSet[A](set: Set[A]) = new RichSet[A](set)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Defines the implicit conversion from an element to a RichSetElement
	 */
	implicit def Element2RichSetElement[A](elem: A) = 
		new RichSetElement[A](elem)
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Defines some constants and methods (mostly with Unicode) that make 
 * working with sets a little more convenient. 
 * @author Michael Cotterell
 */
trait RichSetDefinitions {
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/** 
	 * Returns the empty set
	 */
	final def ∅[A]: Set[A] = Set[A]()
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * If the set contains Numeric elements, returns the product series of 
	 * the set.
	 */
	def ∏[A: Numeric](set: Set[A]): A = 
		set.reduceLeft(implicitly[Numeric[A]] times (_, _))
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * If the set contains Numeric elements, returns the summation series of
	 * the set. 
	 */
	def ∑[A: Numeric](set: Set[A]): A = 
		set.reduceLeft(implicitly[Numeric[A]] plus (_, _))
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether a predicate holds for some of the elements of this set.
	 */
	def ∃[A](p: (A) ⇒ Boolean, set: Set[A]): Boolean = set ∃ p
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether a predicate does not hold for some of the elements of 
	 * this set.
	 */
	def ∄[A](p: (A) ⇒ Boolean, set: Set[A]): Boolean = set ∄ p
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether a predicate holds for all elements of this set.
	 */
	def ∀[A](p: (A) ⇒ Boolean, set: Set[A]): Boolean = set ∀ p
	
}

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
	def ⊂(that: Set[A]): Boolean = set subsetOf that
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether this set is not a subset of another set.
	 */
	def ⊄(that: Set[A]): Boolean = !(set subsetOf that)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether this set is a superset of another set.
	 */
	def ⊃(that: Set[A]): Boolean = that subsetOf set
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether this set is not a superset of another set.
	 */
	def ⊅(that: Set[A]): Boolean = that subsetOf set
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether this set contains an element.
	 */
	def ∋(that: A): Boolean = set contains that
	
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Defines some rich methods (mostly defined with Unicode) that can be used 
 * by an element.
 * @author Michael Cotterell
 */
class RichSetElement[A](elem: A)
{
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Tests whether this element is contained in a set
	 */
	def ∈(set: Set[A]): Boolean = set ∋ elem
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * An example of how to use RichSet
 * @author Michael Cotterell
 */
object RichSetTest extends RichSetDefinitions
{
	
	def main(args: Array[String]): Unit = {
	
		// We can define a set like we do normally
		val x = Set(1, 2, 3, 4)
		val y = Set(0, 2, 5, 4)
		
		// We can test if some value is an element of a set
		println(2 ∈ x)
		println(x ∋ 2)
		
		// We can test if some predicate holds over a set
		println( x ∃ (_ > 10) )
		
		// @TODO see if we can avoid specifying type
		println( ∃[Int](_ > 10, x) )
		
		// We can get the intersection of two sets
		println(x ∩ y)
		println(x ∩ ∅)
		println(∅ ∩ x)
		
		// Should produce a compile error if there's a type mismatch
		// For example, the following won't compile because x is not a Set[String]
		//println("f" ∈ x)
		
		// We can get the summation and product series of a set
		println(∑(x)) // should = 10
		println(∏(x)) // should = 24
		
		
	}
	
}


