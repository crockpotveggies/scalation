/* $Id$ */

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller, Michael Cotterell
 * @see     LICENSE (MIT style license file).
 */

package scalation

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * The ScalaTion trait can be used to quickly grant objects access to 
 * Scalation's core language features.
 * 
 * @author Michael E. Cotterell
 */
trait ScalaTion
{
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	// NOTE: In order to reduce circular dependencies we should probably take
	//       care not to import anything into this trait. Instead, it should
	//       be recommended that we address types, classes, and objects with
	//       their fully qualified namespace name.
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	// Makes certain types immediately available.
	
	type Set[A] = scala.collection.mutable.Set[A]
	type VectorN[A] = advmath.Vec[A] // for legacy support
	type Vec[A] = advmath.Vec[A]
	type MatrixN[A] = advmath.MatrixN[A]

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	// Makes certain factories available immediately.
	
	val Set = scala.collection.mutable.Set
	val VectorN = advmath.Vec // for legacy support
	val Vec = advmath.Vec
	// val MatrixN = scalation.advmath.MatrixN
	// @todo make this available once MatrixN has been refactored
	
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichSet's operations available to Set
	 */
	implicit def MkRichSetOps[A](set: Set[A]) = 
		new rich.RichSet[A](set)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichOrdered's operations available to any ordered type
	 */
	implicit def MkRichOrderedOps[A <% Ordered[A]](elem: A) =
		new rich.RichOrdered[A](elem)
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichAny's operations available to Any
	 */
	implicit def MkRichAnyOps[A](elem: A) = 
		new rich.RichAny[A](elem)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichIntegral's operations available to any integral type
	 */
	implicit def MkRichIntegralOps[A: Integral](elem: A) = 
		new rich.RichIntegral[A](elem)
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichFractional's operations available to any Fractional type
	 */
	implicit def MkRichFractionalOps[A: Fractional](elem: A) = 
		new rich.RichFractional[A](elem)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichNumeric's operations available to any numeric type
	 */
	implicit def MkRichNumericOps[N: Numeric](elem: N) = 
		new rich.RichNumeric[N](elem)
		
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Set summation
	 */
	def ∑[A: Numeric](set: Set[A]): A =
        set.reduceLeft (implicitly[Numeric[A]] plus (_, _))
            
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Vector summation
     */
    def ∑[A: Numeric](vector: Vec[A]): A = vector.sum
 
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Range summation
     */
    def ∑[A <: Range](range: A) = range.toSet.reduceLeft(_+_)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/    	
    /**
     * Summation series 
     */
    def ∑[A: Numeric, B: Numeric](a: Int, b: Int, f: (A) => B): B = {
		val series = for (i <- a to b) yield f(implicitly[Numeric[A]] fromInt i)
		series reduceLeft (implicitly[Numeric[B]] plus (_,_)) 
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/    	
    /**
     * Summation series 
     */
    def ∑[A: Numeric, B: Numeric](r: Range, f: (A) => B): B = {
		val series = for (i <- r) yield f(implicitly[Numeric[A]] fromInt i)
		series reduceLeft (implicitly[Numeric[B]] plus (_,_)) 
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set product
     */
    def ∏[A: Numeric](set: Set[A]): A =
        set.reduceLeft (implicitly[Numeric[A]] times (_, _))
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Vector product
     */
    def ∏[A: Numeric](vector: Vec[A]): A = vector.product
  
      	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Range product
     */
    def ∏[A <: Range](range: A) = range.toSet.reduceLeft(_*_)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/    	
    /**
     * Product series 
     */
    def ∏[A: Numeric, B: Numeric](a: Int, b: Int, f: (A) => B): B = {
		val series = for (i <- a to b) yield f(implicitly[Numeric[A]] fromInt i)
		series reduceLeft (implicitly[Numeric[B]] times (_,_)) 
	}
    
        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/    	
    /**
     * Product series 
     */
    def ∏[A: Numeric, B: Numeric](r: Range, f: (A) => B): B = {
		val series = for (i <- r) yield f(implicitly[Numeric[A]] fromInt i)
		series reduceLeft (implicitly[Numeric[B]] times (_,_)) 
	}
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** 
     * Returns the empty set
     */
    final def ∅[A]: Set[A] = Set[A]()
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** 
     * Positive infinity
     */
    final val ∞ = Double.PositiveInfinity
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** 
     * Negative infinity
     */
    final val -∞ = Double.NegativeInfinity
    	
}