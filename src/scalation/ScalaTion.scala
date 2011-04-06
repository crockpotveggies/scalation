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
	
	implicit def NumericIndexedSeqIsVec[A: Numeric: ClassManifest](seq: IndexedSeq[A]) =
		Vec.fromSeq[A](seq)
	
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
	 * Set summation
	 */    
    def sum[A: Numeric](set: Set[A]): A = ∑(set)
            
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Vector summation
     */
    def ∑[A: Numeric](vector: VectorN[A]): A = vector.sum
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Vector summation
     */
    def sum[A: Numeric](vector: VectorN[A]): A = ∑(vector)
 
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Range summation
     */
    def ∑[A <: Range](range: A) = range.toSet.reduceLeft(_+_)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Range summation
     */
    def sum[A <: Range](range: A) = ∑(range)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/    	
    /**
     * Product series 
     */
    def ∑[A: Numeric, B: Numeric](a: Int, b: Int, f: (A) => B): B = 
    {
		val series = for (i <- a to b) yield f(implicitly[Numeric[A]] fromInt i)
		series reduceLeft (implicitly[Numeric[B]] plus (_,_)) 
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
     * Product series 
     */
    def sum[A: Numeric, B: Numeric](a: Int, b: Int, f: (A) => B): B = 
    	∑(a, b, f)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set product
     */
    def ∏[A: Numeric](set: Set[A]): A =
        set.reduceLeft (implicitly[Numeric[A]] times (_, _))
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set product
     */
    def product[A: Numeric](set: Set[A]): A = ∏(set)
    	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Vector product
     */
    def ∏[A: Numeric](vector: VectorN[A]): A = vector.product
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Vector product with map
     */
    def ∏[A: Numeric, B: Numeric](vector: VectorN[A], f: (A) => B): B = {
    	vector.map(f).product
    }
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Vector product
     */
    def product[A: Numeric](vector: VectorN[A]): A = ∏(vector)
        	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Range product
     */
    def ∏[A <: Range](range: A) = range.toSet.reduceLeft(_*_)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Range product
     */
    def product[A <: Range](range: A) = ∏(range)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/    	
    /**
     * Product series 
     */
    def ∏[A: Numeric, B: Numeric](a: Int, b: Int, f: (A) => B): B = 
    {
		val series = for (i <- a to b) yield f(implicitly[Numeric[A]] fromInt i)
		series reduceLeft (implicitly[Numeric[B]] times (_,_)) 
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
     * Product series 
     */
    def product[A: Numeric, B: Numeric](a: Int, b: Int, f: (A) => B): B = 
    	∏(a, b, f)
    
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