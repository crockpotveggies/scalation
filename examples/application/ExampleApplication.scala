/* $Id$ */

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller, Michael Cotterell
 * @see     LICENSE (MIT style license file).
 */

package application

// This is all we need to import in order to take advantage of the ScalaTion's
// core functionality.
import scalation.ScalaTion

/**
 * Example application that uses ScalaTion! Notice how it extends ScalaTion?
 * That's how all the magic happens.
 * @author Michael Cotterell
 */
object ExampleApplication extends ScalaTion
{
	
	def f(n: Int): Int = n
	
	def section(title: String) {
	    println()
	    println(title.toUpperCase)
	    println(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
	}
	
	def main(args : Array[String]) : Unit = {
		
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Exponentiation */
	    
	    val exp1 = 2↑2		// 4
	    val exp2 = 2↑2↑2	// 16
	    
	    section("Exponentiation")
	    println("2↑2   = %s".format(exp1))
	    println("2↑2↑2 = %s".format(exp2))
	    
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Roots */
	    
	    val root1 = 4↓2				// 2
	    val root2 = 4↓2↓2			// 1.41421...
	    val test  = 4↑0.5 == 4↓2	// true
	    
	    section("Roots")
	    println("4↓2          = %s".format(root1))
	    println("4↓2↓2        = %s".format(root2))
	    println("4↑0.5 == 4↓2 = %s".format(test))
	    
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Factorials */
	    
	    val fac1 = 4!	// 24
	    val fac2 = 3.5!	// 11.63172...
	    
	    section("Factorials")
	    println("4!   = %s".format(fac1))
	    println("3.5! = %s".format(fac2))
		
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Rising and Falling Factorials */
	    
	    val rising  = 4 ⇑ 4 // 840
	    val falling = 4 ⇓ 4 // 24
	    
	    section("Rising and Falling Factorials")
	    println("4 ⇑ 4 = %s".format(rising))
	    println("4 ⇓ 4 = %s".format(falling))
	    
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Product Series */
	    
	    val prod1 = ∏(1 to 3) 						// 6
        val prod2 = ∏(1 to 3, (i: Int) ⇒ i ↑ 2)		// 36
	    
        val vec = Vec(1, 2, 3, 4)
        
        val prod3 = ∏(vec)							// 24
        val prod4 = ∏(0 to 2, (i: Int) ⇒ vec(i))	// 6
        
	    section("Product Series")
	    println("∏(1 to 3)             = %s".format(prod1))
	    println("∏(1 to 3, i ⇒ i ↑ 2)  = %s".format(prod2))
	    println("vec                   = %s".format(vec))
	    println("∏(vec)                = %s".format(prod3))
	    println("∏(0 to 2, i ⇒ vec(i)) = %s".format(prod4))
	    
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Summation Series */
	    
	    val sum1 = ∑(1 to 3) 						// 6
        val sum2 = ∑(1 to 3, (i: Int) ⇒ i ↑ 2)		// 14
        val sum3 = ∑(vec)							// 10
        val sum4 = ∑(0 to 2, (i: Int) ⇒ vec(i))		// 6
        
	    section("Summation Series")
	    println("∑(1 to 3)             = %s".format(sum1))
	    println("∑(1 to 3, i ⇒ i ↑ 2)  = %s".format(sum2))
	    println("vec                   = %s".format(vec))
	    println("∑(vec)                = %s".format(sum3))
	    println("∑(0 to 2, i ⇒ vec(i)) = %s".format(sum4))
	    
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Definite Integral (Approximation) */
	    
	    val int1 = ∫(1 to 4, (i: Double) ⇒ i ↑ 2)	// 21
	    
	    section("Definite Integral (Approximation)")
	    println("∫(1 to 4, i ⇒ i ↑ 2) = %s".format(int1))
	    
	    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
		/* Sets and Set-like Objects */
	    
	    val set1 = Set(1, 2, 3, 4)
	    val set2 = Set(1.0, 2.0, 3.0, 4.0)
	    
	    val setTest1 = 2 ∈ set1		// true
	    val setTest2 = 5 ∈ set1		// false
	    val setTest3 = 2.0 ∈ set2	// true 
	    
	    section("Sets and Set-like Objects")
	    println("set1     = %s".format(set1))
	    println("set2     = %s".format(set2))
	    println("2 ∈ set1 = %s".format(setTest1))
	    println("5 ∈ set1 = %s".format(setTest2))
	    println("2 ∈ set2 = %s".format(setTest3))
	    
	}
}
