package se.sics.cooja.coojatest


import wrappers._

import reactive._

import javax.swing.{JInternalFrame, JTable, JScrollPane}
import java.io.{BufferedWriter, FileWriter, PrintWriter, Writer}
import java.util.{Observer, Observable}

import se.sics.cooja._



package object rules {
  private var assertions = List[Assertion]()
  def assert(s: Signal[Boolean])(implicit sim: Simulation) { 
    assertions ::= new Assertion(s, sim)
  }
  def assert(es: EventStream[Boolean])(implicit sim: Simulation) { 
    assertions ::= new Assertion(es.hold(true), sim)
  }
  
  private var logrules = List[LogRule]()
  def log(logdest: LogDestination, s: Signal[_]*)(implicit sim: Simulation) { 
    logrules ::= new LogRule(logdest, sim, s.toList) 
  }
  def log(logdest: LogDestination, es: EventStream[_])(implicit sim: Simulation) { 
    logrules ::= new LogRule(logdest, sim, es) 
  }

  def reset() {
    assertions = Nil
    logrules = Nil
  }
}

package rules {
  trait Rule extends Observing



  class Assertion(val cond: Signal[Boolean], val sim: Simulation) extends Rule {
    for(c <- cond.distinct) {
      if(c == false) {
        println("ASSERT: " + cond + " is " + c)
        sim.stopSimulation() 
      }
    }
  }



  trait LogDestination {
    def log(time: Long, values: List[_]): Unit
  }

  case class LogFile(file: String, sep: String = "\t")(implicit sim: Simulation) extends LogDestination {
    val stream = new PrintWriter(new BufferedWriter(new FileWriter(file)))
    sim.addObserver(new Observer() {
      def update(obs: Observable, obj: Object) {
        if(!sim.isRunning) stream.flush()
      }
    })

    def log(time: Long, values: List[_]) {
      stream.println(time + sep + values.mkString(sep))
    }
  }

  case class LogWindow(name: String, valueNames: String*)(implicit sim: Simulation) extends LogDestination {
    val window = new JInternalFrame(name, true, true, true, true)

    val model = new javax.swing.table.DefaultTableModel {
      override def isCellEditable(row: Int, col: Int) = false
    }
    model.addColumn("Time")
    if(valueNames.isEmpty)
      model.addColumn("Value")
    else
      valueNames foreach model.addColumn

    val table = new JTable(model)
    
    window.add(new JScrollPane(table))
    window.setSize(300, 500)
    window.show()
    sim.getGUI.getDesktopPane.add(window)

    def log(time: Long, values: List[_]) {
      val v = new java.util.Vector[Object](values.length + 1)
      v.addElement((time/1000.0).asInstanceOf[AnyRef])
      for(x <- values) v.addElement(x.asInstanceOf[AnyRef])
      model.addRow(v)
    }
  }

  class LogRule(val dest: LogDestination, val sim: Simulation, val es: EventStream[_]) extends Rule {
    def this(dest: LogDestination, sim: Simulation, values: List[Signal[_]]) = this(
      dest, sim, values.tail.foldLeft(values.head) {
        case (combined, signal) => signal.flatMap(s => combined)
      }.map {
        x => values.map(_.now.toString)
      }.change
    )

    for(e <- es) {
      dest.log(new RichSimulation(sim).time, e match {
        case list: List[_] => list
        case event: Any => List(event) 
      })
    }
  }
}
