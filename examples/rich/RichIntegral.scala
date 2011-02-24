package rich

import scalation.ScalaTion

/**
 * Example of how to take advantage of RichIntegral
 * @author Michael Cotterell
 */
object RichAny extends ScalaTion
{
	def printSection(title: String)
	{
		println
		for (i <- 1 to 80) print(":"); println
		println(title.toUpperCase)
		for (i <- 1 to 80) print(":"); println
		println
	}
	
	def main(args : Array[String])
	{
	
		println("Example of how to take advantage of RichIntegral")
		
		printSection("Numeric factorial")
		
		println("3! = " + (3!))
		
		// more examples to come
		
	}
}
