
/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Mon Sep  7 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation.process

import scalation.animation._
import scalation.animation.CommandType._
import scalation.scala2d._
import scalation.scala2d.Colors._
import scalation.util.Monitor

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * The Sink class is used to terminate entities (SimActors) when they are finished.
 * @param name  the name of the sink
 * @param at    the location of the sink
 */
class Sink (name: String, at: Array [Double])
           extends Component with Monitor
{
    {
        setName (name)
        setAt (at)
    } // primary constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tell the animation engine to display this Sink.
     */
    def display ()
    {
        director.animate (this, CreateNode, red, Ellipse (), at)
    } // display

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Leave the model, effectively terminating the entity (SimActor).
     */
    def leave ()
    {
        val actor = director.theActor
        tally (0.0)
        trace (this, "terminates", actor, actor.time)
        director.animate (actor, MoveToken, null, null, 
                 Array (at(0) + DIAM, at(1) + at(3) / 2. - RAD))
        actor.yieldToDirector (true)
    } // leave
    
} // Sink

