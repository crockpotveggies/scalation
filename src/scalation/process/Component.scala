
/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Mon Sep  7 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation.process

import scalation.stat.{Statistic, TimeStatistic}
import scalation.util.Identity

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * The Component trait provides basic common feature for simulation components.
 */
trait Component extends Identity
{
    /** Director of the Model (to which the component belongs)
     */
    private var _director: Model = null

    /** Where the component is at (its location)
     */
    private var _at: Array [Double] = null

    /** Collector of sample statistics (e.g., waiting time)
     */
    private val _durationStat = new Statistic ()

    /** Collector of time persistent statistics (e.g., number in queue)
     */
    private val _persistentStat = new TimeStatistic ()

    /** Radius of a token (for animating entities)
     */
    val RAD = 5.

    /** Diameter of a token (for animating entities)
     */
    val DIAM = 2. * RAD

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Director who controls the play.
     */
    def director = _director

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set the name of the director (controlling model).
     * @param director  the director of the play.
     */
    def setDirector (__director: Model)
    {
        if (_director == null && __director != null) {
            _director = __director
        } else {
            flaw ("setDirector", "director may only be set once")
        } // if
    } // setDirector

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Where the component is at.
     */
    def at = _at

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Set the name of the director (controlling model).
     * @param director  the director of the play.
     */
    def setAt (__at: Array [Double])
    {
        if (_at == null && __at != null) {
            _at = __at
        } else {
            flaw ("setAt", "location may only be set once")
        } // if
    } // setAt

    def display (): Unit

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Tally the duration (e.g., waiting time) of an activity or delay.
     * @param duration  the time duration
     */
    def tally (duration: Double) { _durationStat.tally (duration) }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Accumulate the a value (e.g., number in  queue) weighted by its time duration.
     * @param value  the value to accumulate
     * @param time   the current time of the observation
     */
    def accumulate (value: Double, time: Double) { _persistentStat.accumulate (value, time) }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Sample statistics for durations for the component.
     */
    def durationStat = _durationStat

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Persistent statistics for value for the component.
     */
    def persistentStat = _persistentStat

} // Component trait

