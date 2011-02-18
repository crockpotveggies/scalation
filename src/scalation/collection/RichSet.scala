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
        
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Defines the implicit conversion from a range to a Set
     */
    implicit def Range2RichSet[Int](range: Range.Inclusive) = 
    {	
    	def traverse[A](list: List[A])(set: Set[A]): Set[A] = list match {
    		case hd :: tail => traverse(tail)(set + hd)
    		case Nil => set
    	}
 
    	traverse(range.toList)(Set())
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * If the set contains Numeric elements, returns the summation series of
     * the set. 
     */
    object ∑
    {
    	override def toString(): String = "∑"
        def apply[A: Numeric](set: Set[A]): A =
            set.reduceLeft(implicitly[Numeric[A]] plus (_, _))
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * If the set contains Numeric elements, returns the product series of 
     * the set.
     */
    object ∏
    {
    	override def toString(): String = "∏"
        def apply[A: Numeric](set: Set[A]): A =
            set.reduceLeft(implicitly[Numeric[A]] times (_, _))
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** 
     * Returns the empty set
     */
    final def ∅[A]: Set[A] = Set[A]()
        
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
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this set does not contain an element.
     */
    def ∌(that: A): Boolean = !(set contains that)
    
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
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tests whether this element is not contained in a set
     */
    def ∉(set: Set[A]): Boolean = set ∌ elem
}

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * An example of how to use RichSet
 * @author Michael Cotterell
 */
object RichSetTest
{
	
	/**
	 * Cool factorial definition
	 */
	def factorial(n: Int) = ∏(1 to n)
	
    def main(args: Array[String]): Unit = {
    
        // this is how the magic happens
        import scalation.collection.RichSet._
        
        // We can define a set like we do normally
        val x = Set(1, 2, 3, 4)
        val y = Set(0, 2, 5, 4)
        val z = Set(1, 2)
        
        println("val x = " + x)
        println("val y = " + y)
        println("val z = " + z)
        
        // We can test if some value is an element of a set
        println("2 ∈ x = " + 2 ∈ x)
        println("2 ∉ x = " + 2 ∉ x)
        println("x ∋ 2 = " + x ∋ 2)
        println("x ∌ 2 = " + x ∌ 2)
        
        // We can test if some predicate holds over a set
        println("x ∃ (_ > 10) = " + x ∃ (_ > 10) )
        println("x ∃ (_ < 10) = " + x ∃ (_ < 10) )
        println("x ∄ (_ > 10) = " + x ∄ (_ > 10) )
        println("x ∄ (_ < 10) = " + x ∄ (_ < 10) )
        println("x ∀ (_ > 10) = " + x ∀ (_ > 10) )
        println("x ∀ (_ < 10) = " + x ∀ (_ < 10) )
        
        // We can get the intersection of two sets
        println("x ∩ y = " + x ∩ y)
        println("x ∩ ∅ = " + x ∩ ∅)
        println("∅ ∩ x = " + ∅ ∩ x)
        
        // We can get the union of two sets
        println("x ∪ y = " + x ∪ y)
        println("x ∪ ∅ = " + x ∪ ∅)
        println("∅ ∪ x = " + ∅ ∪ x)
        
        // We can check subsets and supersets
        println("x ⊂ x =" + x ⊂ x)
        println("x ⊂ z =" + x ⊂ z)
        println("x ⊄ x =" + x ⊄ x)
        println("x ⊄ z =" + x ⊄ z)
        println("z ⊂ x =" + z ⊂ x)
        println("z ⊄ x =" + z ⊄ x)
        println("x ⊃ z =" + x ⊃ z)
        println("x ⊃ x =" + x ⊃ x)
        println("x ⊅ x =" + x ⊅ x)
        println("x ⊅ z =" + x ⊅ z)
        println("z ⊃ x =" + z ⊃ x)
        println("z ⊅ x =" + z ⊅ x)
        
        // Should produce a compile error if there's a type mismatch
        // For example, the following won't compile because x is not a Set[String]
        //println("f" ∈ x)
        
        // We can get the summation and product series of a set
        println("∑(x) = " + ∑(x)) // should = 10
        println("∏(x) = " + ∏(x)) // should = 24
        
        println("def factorial(n: Int) = ∏(1 to n)")
        println("factorial(4) = " + factorial(4))
        
    }
    
}


