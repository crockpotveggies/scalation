package rich

import scalation.ScalaTion

/**
 * Example of how to take advantage of RichAny
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
	
		println("Example of how to take advantage of RichAny")
		
		printSection("element of set")
		
		val ab = Set("a", "b")
		
		println("ab = " + ab)
		println("a ∈ ab = " + "a" ∈ ab)
		println("a ∉ ab = " + "a" ∉ ab)
		
	}
}
