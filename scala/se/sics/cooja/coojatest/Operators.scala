package se.sics.cooja.coojatest.operators

import reactive._



object Operators {
  def count[A](es: EventSource[A]):Signal[Int] = es.foldLeft(0){ (count, event) => count+1 }.hold(0)
  
  case class AvgState(total: Double, count: Int) { lazy val avg = total/count }
  def avg(es: EventSource[Double]):Signal[Double] = es.foldLeft(AvgState(0, 0)){ case (AvgState(total, count), value) => AvgState(total+value, count+1) }.map(_.avg).hold(0)
}
