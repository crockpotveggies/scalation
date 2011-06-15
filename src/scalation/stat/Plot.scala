
/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * @author  John Miller
 * @version 1.0
 * @date    Sun Nov 15 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file). 
 */

package scalation
package stat

import scala.math._
import scala.swing.{MainFrame, Panel, SimpleGUIApplication}

import advmath._
import scala2d._
import scala2d.Colors._
import scala2d.Shapes._

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * The Plot class takes x and y vectors of data values and plots the (x, y)
 * data points.
 * @param x       the x vector of data values (horizontal)
 * @param y       the y vector of data values (vertical)
 * @param _title  title of the plot
 */
class Plot (x: Vec[Double], y: Vec[Double], _title: String = "Plot")
      extends MainFrame
{
    private val frameSize = new Dimension (600, 600)
    private val frameW    = (round (frameSize.getWidth ())).asInstanceOf [Int]
    private val frameH    = (round (frameSize.getHeight ())).asInstanceOf [Int]
    private val offset    = 50
    private val epsilon   = 1E-9
    private val baseX     = offset
    private val baseY     = frameH - offset
    private val stepsX    = 10
    private val stepsY    = 10
    private val minX      = floor (x.min)
    private val maxX      = ceil (x.max + epsilon)
    private val minY      = floor (y.min)
    private val maxY      = ceil (y.max + epsilon)
    private val deltaX    = maxX - minX
    private val deltaY    = maxY - minY
    private val diameter  = 5
    private val dot       = Ellipse ()
    private val axis      = Line ()

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Create a canvas on which to draw the histogram.
     */
    val canvas = new Panel
    {
        background    = white
        preferredSize = frameSize

        /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /**
         * Paint the canvas by plotting the data points.
         * @param g2d  the high resolution 2D Graphics context 
         */
        override def paintComponent (g2d: Graphics2D)
        {
            super.paintComponent (g2d)
            var x_pos = 0
            var y_pos = 0
            var step  = 0.

            //:: Draw the axes

            g2d.setPaint (black)
            g2d.setStroke (new BasicStroke (2.f))
            axis.setLine (baseX - 1, baseY + 1, baseX + 10 + frameW - 2 * offset, baseY + 1)
            g2d.draw (axis)
            axis.setLine (baseX - 1, offset - 10, baseX - 1, baseY + 1)
            g2d.draw (axis)

            //:: Draw the labels on the axes

            y_pos = baseY + 15
            step  = deltaX / stepsX.asInstanceOf [Double]       // for x-axis
            for (j <- 0 to stepsX) {
                val x_val = clip (minX + j * step)
                x_pos = offset - 8 + j * (frameW - 2 * offset) / stepsX
                g2d.drawString (x_val, x_pos, y_pos)
            } // for

            x_pos = baseX - 30
            step  = deltaY / stepsY.asInstanceOf [Double]       // for y-axis
            for (j <- 0 to stepsY) {
                val y_val = clip (maxY - j * step)
                y_pos = offset + 2 + j * (frameH - 2 * offset) / stepsY
                g2d.drawString (y_val, x_pos, y_pos)
            } // for

            //:: Draw the dots for the data points being plotted

            for (i <- 0 until x.length) {
                val xx = round (x(i) * (frameW - 2 * offset).asInstanceOf [Double])
                x_pos = (xx / deltaX).asInstanceOf [Int] + offset
                val yy = round ((maxY - y(i)) * (frameH - 2 * offset).asInstanceOf [Double])
                y_pos = (yy / deltaY).asInstanceOf [Int] + offset
                dot.setFrame (x_pos, y_pos, diameter, diameter)         // x, y, w, h
                g2d.setPaint (black)
                g2d.fill (dot)
            } // for
        } // paintComponent

    } // canvas Panel

    {
        title    = _title
        contents = canvas
        visible  = true
    } // primary constructor

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert value to string and cut out the first four characters.
     * @param x  the value to convert and cut
     */
    def clip (x: Double): String =
    {
        val s = x.toString 
        s.substring (0, min (s.length, 4))
    } // clip

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /**
     * Convert a Plot vectors to a string.
     */
    override def toString = "Plot (y = " + y + " vs. x = " + x + ")"

} // Plot class

/*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/**
 * This object is used to test the Plot class.
 */
object PlotTest extends App
{
/*
    val x = new Vec[Double] (0., 1., 2., 3.,  4.,  5.,  6., 7., 8., 9., 10.)
    val y = new Vec[Double] (0., 1., 4., 9., 16., 25., 16., 9., 4., 1.,  0.)
*/
    val x = Vec.ofLength [Double] (100)
    val y = Vec.ofLength [Double] (100)
    for (i <- 0 until 100) { x(i) = i / 10.; y(i) = pow (x(i) - 5, 2) }
    val plot = new Plot (x, y)
    println ("plot = " + plot)

} // PlotTest object

