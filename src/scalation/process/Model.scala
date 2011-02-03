
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Mon Sep  7 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation.process

import scala.actors._
import scala.collection.mutable._

import scalation.animation._
import scalation.animation.CommandType._
import scalation.stat._
import scalation.scala2d._
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes._
import scalation.util.{Identity, Monitor, PQueue}

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The Model class maintains a list of components making up the model and
 * controls the flow of entities (SimActors) through the model, following the
 * process-interaction world-view.  It maintains a time-ordered priority queue
 * to activate/re-activate each of the entities.  Each entity (SimActor) is
 * implemeneted as a Scala Actor and therefore runs in its own thread. 
 * @param name  the name of the model
 */
class Model (name: String)
            extends Actor with Component with Monitor
{
    {
        setName (name)
    } // primary constructor

    /** The director's clock that keep track of the current simulation time
     */
    private var _clock = 0.0

    /** The agenda of things to be done (time-ordered activation list)
     */
    private val agenda = new PQueue [SimActor] ()
//  private val agenda = new PriorityQueue [SimActor] ()(Ordering.ordered [SimActor])

    /** Simulation termination flag
     */
    private var simulating = false

    /** List of Components making up the model
     */
    private var parts: List [Component] = null

    /** The currently acting actor (act one at a time)
     */
    private var _theActor: SimActor = null

    /** Animation flag
     */
    private var animating = false

    /** The animation engine
     */
    private val dgAni = new DgAnimator ("Process Animator", black, white)

    /** The animation engine's command queue
     */
    private val aniQ = dgAni.getCommandQueue

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Add the component parts to the model.
     * @param _parts  the component parts
     */
    def addComponents (_parts: List [Component])
    {
        parts = _parts
    } // addComponents

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Current value of the director's clock.
     */
    def clock = _clock

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Current acting actor.
     */
    def theActor = _theActor

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compare the order of actors based on their actTime.
     * @param actor1  the first actor in comparison
     */
    def orderedActor (actor1: SimActor): Ordered [SimActor] =
    {
        new Ordered [SimActor]
            { def compare (actor2: SimActor) = actor1.actTime compare actor2.actTime }
    } // orderedActor

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Start the simulation (includes scheduling all Sources).
     */
    def startSim ()
    {
        trace (this, "initilizes", this, _clock)
        for (p <- parts) {
            trace (this, "establish x = " + p.at(0) + " y = " + p.at(1), p, _clock)
            p.setDirector (this)
            if (p.isInstanceOf [Source]) reschedule (p.asInstanceOf [Source]) 
        } // for
        display ()         // comment out to turn off animation
        simulating = true
        start ()
    } // startSim

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Schedule (first time) or reschedule (sunsequent times) an actor to act.
     * @param actor  the actor to be scheduled
     */
    def reschedule (actor: SimActor) { agenda += actor }

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * The model itself is an Actor (not an ordinary SimActor) and may be
     * thought of as the director.  The director iteratively manages the clock
     * and the agenda of actors until the simulation flag becomes false
     * or the agenda becomes empty.
     */
    def act ()
    {
        trace (this, "starts", this, _clock)

        while (simulating && ! agenda.isEmpty) {
            _theActor = agenda.dequeue ()
            _clock    = _theActor.actTime
            trace (this, "resumes", _theActor, _clock)
            if (_theActor.yetToAct) {
                _theActor.nowActing ()
                _theActor.start ()
            } else {
                _theActor ! "resume acting"
            } // if
            receive { case msg => trace (this, "receives " + msg.toString, this, _clock) }
        } // while

        dgAni.animate (0, 100000)
        trace (this, "terminates", this, _clock)
        report
    } // act

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Report on the statistical results of the simulation.
     */
    def report
    {
        println ("\nstatistics:\t" + durationStat.labels ())
        for (p <- parts) {
            val space = if (p.name.length > 6) ":\t" else ":\t\t"
            println (p.name + space + p.durationStat) 
        } // for
    } // report

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Put the components on the animation engine's queue.
     */
    def display ()
    {
        animating = true
        for (p <- parts) p.display ()
    } // display

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Put a node/token command on the animation queue.
     * @param who    who is being animated
     * @param what   what animation command
     * @param color  the color the node/token
     * @param shape  the shape of the node/token
     * @param at     the location of the node/token
     */
    def animate (who: Identity, what: CommandType.Value, color: Color,
                 shape: Shape, at: Array [Double])
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            aniQ += AnimateCommand (what, eid, shape, label, true, color, at, _clock)
        } // if
    } // animate

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Put a edge command on the animation queue.
     * @param who    who is being animated
     * @param what   what animation command
     * @param color  the color the edge
     * @param shape  the shape of the edge
     * @param from   the location of the origination node
     * @param to     the location of the destination node
     * @param at     the location of the edge (empty array => implicitly determined)
     */
    def animate (who: Identity, what: CommandType.Value, color: Color,
                 shape: Shape, from: Component, to: Component, at: Array [Double] = Array ())
    {
        if (animating) {
            val eid   = who.id
            val label = who.name
            aniQ += AnimateCommand (what, eid, shape, label, true, color, at, _clock, from.id, to.id)
        } // if
    } // animate

} // Model class

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The ModelTest object is used to test the Model class.
 */
object ModelTest extends Application
{
    new SimpleModel ("bank", 10, Uniform (4000, 6000), Uniform (2900, 3100))

    class SimpleModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
          extends Model (name)
    {
        val entry      = new Source ("entry", this, Customer, nArrivals, iArrivalRV, Array (100, 200, 30, 30))
        val door       = new Sink ("door", Array (400, 200, 30, 30))
        val entry2door = new Transport ("entry2door", moveRV, entry, door, .25)
  
        addComponents (List (entry, door, entry2door))

        case class Customer () extends SimActor ("c", this)
        {
            def act ()
            {
                entry2door.move ()
                door.leave ()
            } // act

        } // Customer

        startSim ()

    } // SimpleModel class

} // ModelTest object

