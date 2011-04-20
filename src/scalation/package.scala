/* $Id$ */

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * Core ScalaTion
 */
package object scalation 
{

	/*
	 * @author Michael Cotterell
	 */
	
	/* 
	 * Scala version 2.8 introduces a new scoping construct called package 
	 * objects. They are used to define types, variables, and methods that are 
	 * visible at the level of the corresponding package. 
	 * @see http://programming-scala.labs.oreilly.com/ch07.html
	 */
	
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * 
     */
	type VecI = advmath.Vec[Int]
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * 
     */
	type VecL = advmath.Vec[Long]
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * 
     */
	type VecF = advmath.Vec[Float]
	
	/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * 
     */
	type VecD = advmath.Vec[Double]
	
}