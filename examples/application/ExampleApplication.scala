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
	def main(args : Array[String]) : Unit = {
		
		// VectorN is automatically available for us to use!
		// Let's define some vectors using VectorN's factory.
		
		var vI = VectorN(1, 2, 3, 4)			// VectorN[Int]
		var vD = VectorN(1.0, 2.0, 3.0, 4.0)	// VectorN[Double]
		
		// Mutable sets are also available automatically!
		
		var sI = Set(1, 2, 3, 4)
		var sD = Set(1.0, 2.0, 3.0, 4.0)
		
		println("vI = " + vI)
		println("vD = " + vD)
		println("sI = " + sI)
		println("sD = " + sD)
		
		// Special functions are available. For example, ∏() and ∑()
		
		println("∏(vI) = " + ∏(vI))
		println("∏(vD) = " + ∏(vD))
		println("∏(sI) = " + ∏(sI))
		println("∏(sD) = " + ∏(sD))
		
		println("∑(vI) = " + ∑(vI))
		println("∑(vD) = " + ∑(vD))
		println("∑(sI) = " + ∑(sI))
		println("∑(sD) = " + ∑(sD))
		
	}
}
