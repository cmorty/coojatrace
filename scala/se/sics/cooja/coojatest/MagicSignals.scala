package se.sics.cooja.coojatest.magicsignals

import reactive._
import scala.util.DynamicVariable



class DepLogger(var deps:List[Signal[_]])



class DynDepLog extends DynamicVariable[DepLogger](new DepLogger(Nil))



object MagicSignals {
  implicit def wrap[T](f: => T)(implicit deplog: DynDepLog): Signal[T] = {
    val deplogger = new DepLogger(Nil)
    deplog.withValue(deplogger) {
      val v0 = f
    }
    deplogger.deps.tail.foldLeft(deplogger.deps.head.map(s => f)) {
      case (combined, signal) => signal.flatMap(s => combined)
    }
  }

  implicit def unwrap[T](s: Signal[T])(implicit deplog: DynDepLog): T = {
    deplog.value.deps ::= s
    s.now
  }
}
