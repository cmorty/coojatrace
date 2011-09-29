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
  operators.DeltaOperator with
  operators.ZipOperator with
  operators.WithTimeOperator with
  operators.WithPositionOperator with
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
 * Delta operator.
 */
trait DeltaOperator {
  /**
   * Computes value differences received from eventstream.
   * @param es [[EventStream]] for whose values deltas are computed
   * @return [[EventStream]] of deltas
   * @tparam T type of eventstream (must be implicitly convertable to Long)
   */
  def delta[T <% Long](es: EventStream[T]): EventStream[Long] = {
    es.foldLeft((0L, 0L)) {
      case ( (last, _), curr ) => (curr, curr - last)
    }.map(_._2)
  }
}

/**
 * Zip operator.
 */
trait ZipOperator {
  /**
   * Turns all given signals into a signal of a list of their values.
   * Order of values in list signal matches argument ordering.
   * A change in '''any''' of the source signals will change the list signal.
   * @param signals one or more [[Signal]]s to turn into list signal
   * @return [[Signal]] of list of input signal values
   * @tparam T type of input signals and output signal list
   */
  def zip[T](signals: Signal[T]*): Signal[List[T]] = {
    require(signals.size > 0)
    val sigs = signals.reverse
    sigs.tail.foldLeft(sigs.head.map(v => List[T](v))) {
      case (combined, signal) => signal.flatMap(s => combined.map(v => s :: v))
    }
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
 * Position-adding operator.
 */
trait WithPositionOperator {
  /**
   * Adds position number to event stream.
   *
   * @param es [[EventStream]] to number
   * @return [[EventStream]] of (numer, value) tuples
   * @tparam T type of eventstream
   */
  def withPosition[T](es: EventStream[T]):EventStream[Tuple2[Int, T]] = {
    es.foldLeft((-1, null.asInstanceOf[T])) {
      case ( (count, last), event ) => (count+1, event)
    }
  }
}

/**
 * Window operator.
 */
trait WindowOperator { this: WithPositionOperator with WithTimeOperator =>
   /**
   * Applies a sliding window to event stream and returns a stream of windows.
   * Each window is returned as a list of all corresponding values.
   *
   * @param es [[EventStream]] over which to "slide"
   * @param start function which checks for last start and current value if new windows should be
   *   started
   * @param stop function whick checks for start and current value if window (with this start
   *   value) should be closed
   * @return [[EventStream]] of window lists
   * @tparam T type of eventstream
   */
  def window[T](es: EventStream[T], start: (T, T) => Boolean, stop: (T, T) => Boolean)(implicit observing: Observing): EventStream[List[T]] = {
    // output event stream
    val outStream = new EventSource[List[T]]

    // last start value      
    var lastStart = null.asInstanceOf[T]
     
    // for each value where window starts...
    for(event <- es.filter(event => start(lastStart, event))) {
      lastStart = event

      // create new list buffer and add first value
      val window = new collection.mutable.ListBuffer[T]
      window.append(event)

      // for the next values until stop
      var done = false
      for(e <- es.takeWhile(e => done == false) ) {
        if(stop(event, e)) {
          // fire complete window list if window end is reached
          done = true
          outStream fire window.toList
        } else {
          // append to buffer
          window.append(e)
        }
      }
    }
    
    // return output stream
    outStream
  }


  /**
   * Applies a sliding position window to event stream and returns a stream of windows.
   * Each window is returned as a list of all corresponding values.
   *
   * @param es [[EventStream]] over which to "slide"
   * @param range size of one window (number of values contained in one window)
   * @oaram slide "space" between two windows (number of values between two windows) 
   * @param offset number of values to wait before starting first window
   * @return [[EventStream]] of window lists
   * @tparam T type of eventstream
   */
  def posWindow[T](es: EventStream[T], range: Int, slide: Int, offset: Int)(implicit observing: Observing): EventStream[List[T]] = {
    require(offset >= 0)
    require(range > 0)
    require(slide > 0)

    // check for start position
    val start: (Tuple2[Int, T], Tuple2[Int, T]) => Boolean = {
      case (_, (pos, _)) => (pos >= offset) && ((pos - offset) % slide == 0)
    }

    // check for end position
    val end: (Tuple2[Int, T], Tuple2[Int, T]) => Boolean = {
      case ((startpos, _), (pos, _)) => pos >= startpos + range
    }

    // apply window(...) on stream with positions
    for(win <- window(withPosition(es), start, end))
      // and strip positions afterwards from each result window
      yield( for( (pos, value) <- win) yield value )
  }


  /**
   * Applies a sliding time window to event stream and returns a stream of windows.
   * Each window is returned as a list of all corresponding values.
   *
   * @param es [[EventStream]] over which to "slide"
   * @param range size of one window (microseconds between first and last value in one window)
   * @oaram slide "space" between two windows (microseconds between two windows) 
   * @param offset time in microseconds wait before starting first window
   * @return [[EventStream]] of window lists
   * @tparam T type of eventstream
   */
  def timeWindow[T](es: EventStream[T], range: Long, slide: Long, offset: Long)(implicit observing: Observing, sim: Simulation): EventStream[List[T]] = {
    require(offset >= 0)
    require(range > 0)
    require(slide > 0)

    // check for start time
    val start: (Tuple2[Long, T], Tuple2[Long, T]) => Boolean = {
      case ((lastTime, _), (time, _)) => (time - lastTime >= slide)
      case (null, (time, _)) => (time >= offset)
    }

    // check for end time
    val end: (Tuple2[Long, T], Tuple2[Long, T]) => Boolean = {
      case ((starttime, _), (time, _)) => time >= starttime + range
    }

    // apply window(...) on stream with times
    for(win <- window(withTime(es), start, end))
      // and strip times afterwards from each result window
      yield( for( (time, value) <- win) yield value )
  }


  /**
   * Applies a sliding absolute time window to event stream and returns a stream of windows.
   * Each window is returned as a list of all corresponding values.
   * "Absolute time" means start time of next window is not influenced by end time of last.
   *
   * @param es [[EventStream]] over which to "slide"
   * @param range size of one window (microseconds between first and last value in one window)
   * @oaram slide "space" between two windows (microseconds between two windows) 
   * @param offset time in microseconds wait before starting first window
   * @return [[EventStream]] of window lists
   * @tparam T type of eventstream
   */
  def absoluteTimeWindow[T](es: EventStream[T], range: Long, slide: Long, offset: Long)(implicit observing: Observing, sim: Simulation): EventStream[List[T]] = {
    require(offset >= 0)
    require(range > 0)
    require(slide > 0)

    // check for start time
    val start: (Tuple2[Long, T], Tuple2[Long, T]) => Boolean = {
      case ((lastTime, _), (time, _)) => 
        ((time-offset) / slide)*slide > ((lastTime-offset) / slide)*slide
      case (null, (time, _)) => (time >= offset)
    }

    // check for end time
    val end: (Tuple2[Long, T], Tuple2[Long, T]) => Boolean = {
      case ((starttime, _), (time, _)) => 
        time >= ((starttime-offset)/slide)*slide + offset + range
    }

    // apply window(...) on stream with times
    for(win <- window(withTime(es), start, end))
      // and strip times afterwards from each result window
      yield( for( (time, value) <- win) yield value )
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

} // package operators
