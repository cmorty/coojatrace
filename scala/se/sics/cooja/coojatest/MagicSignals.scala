package se.sics.cooja.coojatest.magicsignals

import reactive._
import scala.util.DynamicVariable



class DepLogger(var deps:List[Signal[_]])



class DynDepLog extends DynamicVariable[DepLogger](new DepLogger(Nil))



object MagicSignals {
  //implicit def anyValToVal[A <: AnyVal](i: A) = new Val[A](i)
  
  implicit def wrap[T](f: => T)(implicit deplog: DynDepLog): Signal[T] = {
    val deplogger = new DepLogger(Nil)
    deplog.withValue(deplogger) {
      val v0 = f
    }
    println("wrapping " + f + ", deps: " + deplogger.deps)
    deplogger.deps.tail.foldLeft(deplogger.deps.head.map(s => f)) {
      case (combined, signal) => signal.flatMap(s => combined)
    }
  }

  implicit def unwrap[T](s: Signal[T])(implicit deplog: DynDepLog): T = {
    println("unwrapping " + s)
    deplog.value.deps ::= s
    s.now
  }
}
