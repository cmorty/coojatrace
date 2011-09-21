package se.sics.cooja.coojatrace



import reactive._

import se.sics.cooja._



/**
 * Operators to consolidate data while running the simulation.
 */
package object operators extends
  operators.CountOperator with
  operators.AverageOperator with
  operators.StdDevOperator with
  operators.MaximumOperator with
  operators.MinimumOperator with
  operators.WithTimeOperator with
  operators.WindowOperator

package operators {
  /**
   * Count operator.
   */
  trait CountOperator {
    /**
     * Count the number of events received from eventstream.
     * @param es [[EventStream]] whose events are counted
     * @return [[Signal]] of event count, starts at 0 before first event is received
     * @tparam A type of eventstream (works with all types)
     */
    def count[A](es: EventStream[A]): Signal[Int] = {
      es.foldLeft(0) {
       (count, event) => count+1
      }.hold(0)
    }
  }

  /**
   * Average operator state class.
   * @param total sum of all values to average
   * @param count number of values averaged
   */
  private case class AvgState(total: Double, count: Int)
  
  /**
   * Average operator.
   */
  trait AverageOperator {
    /**
     * Average the values received from eventstream.
     * @param es [[EventStream]] whose values are averaged
     * @return [[Signal]] of average, starts with NaN (!) before first value is received
     * @tparam T type of eventstream (must be implicitly convertable to Double)
     */
    def avg[T <% Double](es: EventStream[T]): Signal[Double] = {
      es.foldLeft(AvgState(0, 0)) {
        case (AvgState(total, count), value) => AvgState(total+value, count+1)
      }.map(a => a.total/a.count).hold(Double.NaN)
    }
  }

  /**
   * Standard deviation operator state class.
   * @see [[http://en.wikipedia.org/wiki/Standard_deviation#Rapid_calculation_methods]]
   */
  private case class StdDevState(a: Double, q: Double, i: Int)
  
  /**
   * (Population) standard deviation operator.
   */
  trait StdDevOperator {
    /**
     * Compute the (population) standard deviation of the values received from eventstream.
     * @param es [[EventStream]] for whose values standard deviation is computed
     * @return [[Signal]] of standard deviation, starts with NaN (!) before first value is received
     * @tparam T type of eventstream (must be implicitly convertable to Double)
     * @see [[http://en.wikipedia.org/wiki/Standard_deviation#Rapid_calculation_methods]]
     */
    def stdDev[T <% Double](es: EventStream[T]): Signal[Double] = {
      es.foldLeft(StdDevState(0, 0, 0)) {
        case (StdDevState(a, q, i), value) => {
          val ni = i + 1
          val na = a + (value - a)/ni
          val nq = q + (value - a)*(value - na) 
          StdDevState(na, nq, ni)
        } 
      }.map(s => s.q / s.i).hold(Double.NaN)
    }
  }

  /**
   * Maximum operator.
   */
  trait MaximumOperator {
    /**
     * Compute the maximum of the values received from eventstream.
     * @param es [[EventStream]] for whose values the maximum is computed
     * @return [[Signal]] of maximum, starts with -Infinity(!) before first value is received
     * @tparam T type of eventstream (must be implicitly convertable to Double)
     */
    def max[T <% Double](es: EventStream[T]): Signal[Double] = {
      val minimum = Double.NegativeInfinity
      es.foldLeft(minimum) {
        (maximum, event) => if(event > maximum) event else maximum
      }.hold(minimum)
    }
  }

  /**
   * Minimum operator.
   */
  trait MinimumOperator {
    /**
     * Compute the minimum of the values received from eventstream.
     * @param es [[EventStream]] for whose values the minimum is computed
     * @return [[Signal]] of minimum, starts with +Infinity(!) before first value is received
     * @tparam T type of eventstream (must be implicitly convertable to Double)
     */
    def min[T <% Double](es: EventStream[T]): Signal[Double] = {
      val maximum = Double.PositiveInfinity
      es.foldLeft(maximum) {
        (minimum, event) => if(event < minimum) event else minimum
      }.hold(maximum)
    }
  }

  /**
   * Time-adding operator.
   */
  trait WithTimeOperator {
    /**
     * Add the current simulation time to an event stream by turning each elemnt into a
     * (time, value) tuple
     * @param es [[EventStream]] to be timed
     * @return [[EventStream]] of (time, value) tuples
     * @tparam T type of eventstream
     */
    def withTime[T](es: EventStream[T])(implicit sim: Simulation): EventStream[Tuple2[Long, T]] = {
      es.map(e => (sim.getSimulationTime, e))
    }
  }

  /**
   * Window operator.
   */
  trait WindowOperator {
    /**
     * Adds position number to event stream.
     *
     * @param es [[EventStream]] to number
     * @return [[EventStream]] of (numer, value) tuples
     * @tparam T type of eventstream
     */
    private def withPosition[T](es: EventStream[T]) = es.foldLeft((-1, null.asInstanceOf[T])) {
      case ((count, last), event) => (count+1, event)
    }

    /**
     * Applies a sliding window to event stream and returns a stream of windows.
     * Each window is returned as a list of all corresponding values.
     *
     * @param es [[EventStream]] over which to "slide"
     * @param range size of one window (number of values contained in one window)
     * @oaram slide "space" between two windows (number of values between two windows) 
     * @param offset number of values to wait before starting first window
     * @return [[EventStream]] of window lists
     * @tparam T type of eventstream
     */
    def window[T](es: EventStream[T], range: Int, slide: Int, offset: Int)(implicit observing: Observing): EventStream[List[T]] = {
      require(offset >= 0)
      require(range > 0)
      require(slide > 0)

      // output event stream
      val outStream = new EventSource[List[T]]

      val posStream = withPosition(es)
      
      // for each value...
      posStream.foreach { case (pos, event) =>
        // if a new window starts...
        if( (pos >= offset) && ((pos - offset) % slide == 0) ) {
          // create new list buffer and add first value
          val window = new collection.mutable.ListBuffer[T]
          window.append(event)

          // for the next range values
          for( (p, e) <- posStream.takeWhile(_._1 <= pos + range) ) {
            // append to buffer
            window.append(e)

            // and fire complete window list if window end is reached
            if(p == pos+range-1) outStream fire window.toList
          }
        }
      }
      
      // return output stream
      outStream
    }

    /**
     * Applies a sliding window to event stream and returns a stream of streams.
     * Each stream fires its corresponding values from original stream.
     * 
     * '''Note:''' offset must be at least 1, as there is no way to receive the
     * first event stream AND its first event at the same time.
     *
     * @param es [[EventStream]] over which to "slide"
     * @param range size of one window (number of values contained in one window)
     * @oaram slide "space" between two windows (number of values between two windows) 
     * @param offset number of values to wait before starting first window
     * @return [[EventStream]] of window lists
     * @tparam T type of eventstream
     */
    def windowStream[T](es: EventStream[T], range: Int, slide: Int, offset: Int)(implicit observing: Observing):
        EventStream[EventStream[T]] = {
      require(offset > 0)
      require(range > 0)
      require(slide > 0)

      val posStream = withPosition(es)

      // for each value...
      posStream.collect {
        // if NEXT position starts new window...
        case (pos, event) if( (pos+1>= offset) && ((pos+1-offset) % slide == 0) ) =>
          // return a stream of next range values
          posStream.takeWhile(_._1 <= pos + range).map(_._2)
      }
    }
  }

}
