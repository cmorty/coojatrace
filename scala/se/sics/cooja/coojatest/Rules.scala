package se.sics.cooja.coojatest


import wrappers._

import reactive._

import javax.swing.{JInternalFrame, JTextArea}
import java.io.{BufferedWriter, FileWriter, PrintWriter, Writer}
import java.util.{Observer, Observable}

import se.sics.cooja._



package object rules {
  private var assertions = List[Assertion]()
  def assert(s:Signal[Boolean])(implicit sim: Simulation) { 
    assertions ::= new Assertion(s, sim)
  }
  
  private var logrules = List[LogRule]()
  def log(logdest: LogDestination, s: Signal[_]*)(implicit sim: Simulation) { 
    logrules ::= new LogRule(logdest, sim, s.toList) 
  }

  def reset() {
    assertions = Nil
    logrules = Nil
  }
}

package rules {
  trait Rule extends Observing



  class Assertion(val cond: Signal[Boolean], val sim: Simulation) extends Rule {
    for(c <- cond.distinct.change if c == false) {
      println("ASSERT: " + cond + " is " + c)
      sim.stopSimulation() 
    }
  }



  trait LogDestination {
    val stream: PrintWriter
  }

  case class LogFile(file: String)(implicit sim: Simulation) extends LogDestination {
    val stream = new PrintWriter(new BufferedWriter(new FileWriter(file)))
    sim.addObserver(new Observer() {
      def update(obs: Observable, obj: Object) {
        if(!sim.isRunning) stream.flush()
      }
    })
  }

  case class LogWindow(name: String)(implicit sim: Simulation) extends LogDestination {
    val window = new JInternalFrame(name, true, true, true, true)
    val scriptResult = new JTextArea()
    scriptResult.setEditable(false)
    window.add(scriptResult)
    window.setSize(300, 500)
    window.show()
    sim.getGUI.getDesktopPane.add(window)

    val stream = new PrintWriter(new Writer () { 
      def close {}
      def flush {}
      def write(cbuf: Array[Char],  off: Int, len: Int) {
        scriptResult.append(new String(cbuf, off, len));
      }
    })
  }

  class LogRule(val dest: LogDestination, val sim: Simulation, val values: List[Signal[_]]) extends Rule {
    values.tail.foldLeft(values.head.map(s => dest.stream.println(new RichSimulation(sim).time + "\t" + values.map(_.now).mkString("\t")))) {
      case (combined, signal) => signal.flatMap(s => combined)
    } 
  }
}
