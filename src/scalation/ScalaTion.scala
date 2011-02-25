/* $Id$ */

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
	type VectorN[A] = advmath.VectorN[A]
	type MatrixN[A] = advmath.MatrixN[A]

	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	// Makes certain factories avilable immediately.
	
	val Set = scala.collection.mutable.Set
	val VectorN = advmath.VectorN
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
	 * Makes RichAny's operations available to Any
	 */
	implicit def MkRichAnyOps[A](elem: A) = 
		new rich.RichAny[A](elem)
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
	 * Makes RichIntegral's operations available to any type
	 */
	implicit def MkRichIntegralOps[A: Integral](elem: A) = 
		new rich.RichIntegral[A](elem)
	
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
     * Summation series 
     */
    def ∑[A: Numeric](a: Int, b: Int, f: (Int) => A): A = 
    {
		val series = for (i <- a to b) yield f(i)
		series reduceLeft (implicitly[Numeric[A]] plus (_,_)) 
	}
    
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
     * Summation series 
     */
	def sum[A: Numeric](a: Int, b: Int, f: (Int) => A): A = ∑(a, b, f)
	
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
    def ∏[A: Numeric](a: Int, b: Int, f: (Int) => A): A = 
    {
		val series = for (i <- a to b) yield f(i)
		series reduceLeft (implicitly[Numeric[A]] times (_,_)) 
	}
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/**
     * Product series 
     */
    def product[A: Numeric](a: Int, b: Int, f: (Int) => A): A = ∏(a, b, f)
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /** 
     * Returns the empty set
     */
    final def ∅[A]: Set[A] = Set[A]()
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * The String representation of the Summation series character
     */
    final val ∑ = "∑"
    
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * The String representation of the Product series character
     */
    final val ∏ = "∏"
    	
}