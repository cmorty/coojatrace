package se.sics.cooja.coojatest

import reactive._



package object operators extends
  operators.CountOperator with
  operators.AverageOperator with
  operators.StdDevOperator with
  operators.MaximumOperator with
  operators.MinimumOperator

package operators {
  trait CountOperator {
    def count[A](es: EventStream[A]): Signal[Int] = {
      es.foldLeft(0) {
       (count, event) => count+1
      }.hold(0)
    }
  }

  case class AvgState(total: Double, count: Int)
  trait AverageOperator {
    def avg[T <% Double](es: EventStream[T]): Signal[Double] = {
      es.foldLeft(AvgState(0, 0)) {
        case (AvgState(total, count), value) => AvgState(total+value, count+1)
      }.map(a => a.total/a.count).hold(Double.NaN)
    }
  }

  case class StdDevState(a: Double, q: Double, i: Int)
  trait StdDevOperator {
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

  trait MaximumOperator {
    def max[T <% Double](es: EventStream[T]): Signal[Double] = {
      val minimum = Double.NegativeInfinity
      es.foldLeft(minimum) {
        (maximum, event) => if(event > maximum) event else maximum
      }.hold(minimum)
    }
  }

  trait MinimumOperator {
    def min[T <% Double](es: EventStream[T]): Signal[Double] = {
      val maximum = Double.PositiveInfinity
      es.foldLeft(maximum) {
        (minimum, event) => if(event < minimum) event else minimum
      }.hold(maximum)
    }
  }
}
