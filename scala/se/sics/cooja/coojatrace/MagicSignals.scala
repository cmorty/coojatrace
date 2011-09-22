package se.sics.cooja.coojatrace.magicsignals

import reactive._
import scala.util.DynamicVariable



/**
 * Tracks dependencies for a MagicSignal.
 */
trait DepLogger {
  /**
   * Add a dependency signal to list.
   * @param s dependency signal
   */
  def addDependency(s: Signal[_])
}



/**
 * Dynamic variable which points to the current [[se.sics.cooja.coojatrace.magicsignals.DepLogger]].
 */
class DynamicDepLogger extends DynamicVariable[DepLogger](new DepLogger {
  def addDependency(s: Signal[_]) {} // no dependency tracking when simulating
})



/**
 * Methods for creating a "MagicSignal".
 *
 * "Magic" signals are created automatically by implicit wraps and unwraps to simplify
 * test code because operations can be done on signals as they could be on their raw values.
 * 
 * '''Example:'''
 * {{{ intSignal*5 + stringSignal.length }}}
 *  can be written instead of 
 * {{{ for(s <- stringSignal; i <- intSignal) yield(i*5 + s.length) }}}
 */
object MagicSignals {
  /**
   * Implicitly wrap an expression into a signal of the expressions' result type. Every signal
   * which is implicitly unwrapped will be added as a dependency for this new signal so that
   * dependency value changes are correctly updated.
   *
   * '''Note:''' this method is called automatically by the compiler if an operation which is only
   * implemented for the value of a signal is called on the signal itself.
   *
   * @param f named parameter (function) which is to be turned into a signal
   * @param deplog [[se.sics.cooja.coojatrace.magicsignals.DynDepLog]] which will be pointed 
   *   to a new [[se.sics.cooja.coojatrace.magicsignals.DepLogger]]
   * @return [[Signal]] created by reapplying f at every change of the dependency signals
   * @tparam T result type of f / type of newly created signal
   */
  implicit def wrap[T](f: => T)(implicit deplog: DynamicDepLogger): Signal[T] = {
    // create a new DepLogger to track dependencies
    val deps = collection.mutable.ListBuffer[Signal[_]]()
    val deplogger = new DepLogger {
      def addDependency(s: Signal[_]) { deps.append(s) }
    }

    // evaluate f with the global deplog pointing to our new deplogger
    deplog.withValue(deplogger) {
      val v0 = f
    }

    
    if(deps.isEmpty) {
      // no dependencies found? return a Val but warn as well
      val logger = org.apache.log4j.Logger.getLogger(this.getClass)
      logger.warn("no dependencies found when wrapping value " + f + ", creating Val instead")
      Val(f)
    } else {
      // flatmap all dependency signals into one new signal, whose value is computed
      // by reevaluating f at every change
      deps.tail.foldLeft(deps.head.map(s => f)) {
        case (combined, signal) => signal.flatMap(s => combined)
      }
    }
  }

  /**
   * Implicitly unwrap a signal to its value and save this signal as a dependency
   * for the current magicsignal wrap operation.
   *
   * '''Note:''' this method is called automatically by the compiler if an operation which is only
   * implemented for the value of a signal is called on the signal itself.
   *
   * @param s [[Signal]] which is unwrapped
   * @param deplog [[se.sics.cooja.coojatrace.magicsignals.DynDepLog]] which points to
   *   the currently used [[DepLogger]]
   * @return current value of signal
   * @tparam T type of signal value
   */
  implicit def unwrap[T](s: Signal[T])(implicit deplog: DynamicDepLogger): T = {
    // add to dependency list
    deplog.value.addDependency(s)

    // return current value
    s.now
  }
}
