package se.sics.cooja.coojatest.magicsignals

import reactive._
import scala.util.DynamicVariable



/**
 * Holds a dependency list for a MagicSignal.
 * @param deps list of dependencies
 */
class DepLogger(var deps:List[Signal[_]])



/**
 * Dynamic variable which points to the current [[se.sics.cooja.coojatest.magicsignals.DepLogger]].
 */
class DynDepLog extends DynamicVariable[DepLogger](new DepLogger(Nil))



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
   * @param deplog [[se.sics.cooja.coojatest.magicsignals.DynDepLog]] which will be pointed 
   *   to a new [[se.sics.cooja.coojatest.magicsignals.DepLogger]]
   * @return [[Signal]] created by reapplying f at every change of the dependency signals
   * @tparam T result type of f / type of newly created signal
   */
  implicit def wrap[T](f: => T)(implicit deplog: DynDepLog): Signal[T] = {
    // create a new DepLogger to track dependencies
    val deplogger = new DepLogger(Nil)

    // evaluate f with the global deplog pointing to our new deplogger
    deplog.withValue(deplogger) {
      val v0 = f
    }

    // flatmap all dependency signals into one new signal, whose value is computed
    // by reevaluating f at every change
    deplogger.deps.tail.foldLeft(deplogger.deps.head.map(s => f)) {
      case (combined, signal) => signal.flatMap(s => combined)
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
   * @param deplog [[se.sics.cooja.coojatest.magicsignals.DynDepLog]] which points to
   *   the currently used [[DepLogger]]
   * @return current value of signal
   * @tparam T type of signal value
   */
  implicit def unwrap[T](s: Signal[T])(implicit deplog: DynDepLog): T = {
    // add to dependency list
    deplog.value.deps ::= s

    // return current value
    s.now
  }
}
