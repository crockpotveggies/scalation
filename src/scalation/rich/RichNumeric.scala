package scalation.rich

class RichNumeric[N: Numeric](elem: N) {

	private val evidence = implicitly[Numeric[N]]
	
}