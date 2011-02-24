/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Core ScalaTion
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
	// Makes certain types immediately available.
	
	type Set[A] = scala.collection.mutable.Set[A]
	type VectorN[A] = scalation.advmath.VectorN[A]
	type MatrixN[A] = scalation.advmath.MatrixN[A]
	

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	// Makes certain factories avilable immediately.
	
	val Set = scala.collection.mutable.Set
	val VectorN = scalation.advmath.VectorN
	// val MatrixN = scalation.advmath.MatrixN
	// @todo make this available once MatrixN has been refactored
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichSet's operations available to Set
	 */
	implicit def MkRichSetOps[A](set: Set[A]) = 
		new scalation.rich.RichSet[A](set)
	
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichAny's operations available to Any
	 */
	implicit def MkRichAnyOps[A](elem: A) = 
		new scalation.rich.RichAny[A](elem)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichIntegral's operations available to any type
	 */
	implicit def MkRichIntegralOps[A: Integral](elem: A) = 
		new scalation.rich.RichIntegral[A](elem)
	
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
    def ∑[A: Numeric](vector: VectorN[A]): A = vector.sum
 
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Range summation
     */
    def ∑[A <: Range](range: A) = range.toSet.reduceLeft(_+_)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Summation series 
     */
    def ∑[A: Numeric](a: Int, b: Int, f: (Int) => A): A = {
		val series = for (i <- a to b) yield f(i)
		series reduceLeft (implicitly[Numeric[A]] plus (_,_)) 
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
    def ∏[A: Numeric](vector: VectorN[A]): A = vector.product
        	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Range product
     */
    def ∏[A <: Range](range: A) = range.toSet.reduceLeft(_*_)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/    	
    /**
     * Product series 
     */
    def ∏[A: Numeric](a: Int, b: Int, f: (Int) => A): A = {
		val series = for (i <- a to b) yield f(i)
		series reduceLeft (implicitly[Numeric[A]] times (_,_)) 
	}
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** 
     * Returns the empty set
     */
    final def ∅[A]: Set[A] = Set[A]()
    
    /**
     * The string representation of the empty set
     */
    final val ∅ = "∅"
    
    /**
     * The String representation of the Summations series character
     */
    final val ∑ = "∑"
    	
    /**
     * The String representation of the Product series character
     */
    final val ∏ = "∏"
    	
}