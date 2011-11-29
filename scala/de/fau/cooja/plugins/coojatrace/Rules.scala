/*
 * Copyright (c) 2011, Florian Lukas
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 2. Redistributions in
 * binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package de.fau.cooja.plugins.coojatrace



import wrappers._

import reactive._

import javax.swing.{JInternalFrame, JTable, JScrollPane}
import java.io.{BufferedWriter, FileWriter, PrintWriter, Writer}
import java.util.{Observer, Observable}

import se.sics.cooja._


class Rules{}

package rules {



package object assertions {
  /**
   * Create new assertion from (boolean) [[Signal]].
   * Assertion is raised when signal changes to `false`.
   * @param cond [[Signal]]`[Boolean]` to check
   * @param name name of assertion. Will be printed when raised
   * @param sim the current [[Simulation]]
   * @param obs the observing context implicit
   */
  def assert(cond: Signal[Boolean], name: String)(implicit sim: Simulation, obs: Observing) { 
    // only check on actual changes
    for(c <- cond.distinct) {
      // assertion violated
      if(c == false) {
        // log and stop
        CoojaTracePlugin.forSim(sim).logger.info("ASSERTION: " + name + " is " + c)
        sim.stopSimulation() 

        if(GUI.isVisualized) {
          // show dialog if visualized
          javax.swing.SwingUtilities.invokeLater(new Runnable() {
            def run() {
              javax.swing.JOptionPane.showMessageDialog(GUI.getTopParentContainer,
                "The following assertion failed and stopped the simulation:\n\n" + name,
                "Assertion failed", javax.swing.JOptionPane.INFORMATION_MESSAGE)
            }
          })          
        } else {
          // quit cooja if not visualized (code from LogScriptEngine plugin)
          new Thread() {
            override def run() {
              try { Thread.sleep(500) } catch { case e: InterruptedException => }
              sim.getGUI.doQuit(false)
            }
          }.start()

          new Thread() {
            override def run() {
              try { Thread.sleep(2000) } catch { case e: InterruptedException => }
              CoojaTracePlugin.forSim(sim).logger.warn("Killing COOJA")
              System.exit(1)
            }
          }.start()
        }
      }
    }
  }

  /**
   * Create new assertion from (boolean) [[EventStream]].
   * Assertion is raised `false` is received.
   * @param es [[EventStream]]`[Boolean]` to check
   * @param name name of assertion. Will be printed when raised
   * @param sim the current [[Simulation]]
   * @param obs the observing context implicit
   */
  def assert(es: EventStream[Boolean], name: String)(implicit sim: Simulation, obs: Observing) { 
    // convert stream to signal and call assert for signals
    assert(es.hold(true), name)
  }
}




package object logrules {
  /**
   * Create new rule to log one or more signals.
   * @param to [[LogDestination]]-object where values shall be logged to
   * @param sig [[Signal]]s to be logged. If multiple signals are given, a change in any 
   * of the signals will generate a new log-line listing ´´´all´´´ current values
   * @param sim the current [[Simulation]]
   * @param obs the observing context implicit
   */
  def log(to: LogDestination, sig: Signal[_]*)(implicit sim: Simulation, obs: Observing) { 
    require(sig.size > 0)

    val signals = sig.reverse
    val stream = signals.tail.foldLeft(signals.head.map(v => List(v.toString))) {
      case (combined, signal) => signal.flatMap(s => combined.map(v => s.toString :: v))
    }.change

    // call dest.log for every change with a list of current values (as long as to is active)
    stream.takeWhile(_ => to.active).foreach(to.log)
  }

  /**
   * Create new rule to log an eventstream and any number of signals to sample with.
   * @param to [[LogDestination]]-object where values shall be logged to
   * @param es [[EventStream]] to be logged. Multiple columns can be logged by passing an event
   *   strean of 
   * @param sig (optional) [[Signal]]s which will be sampled at every new event from es. Signal
   *   values will '''not''' be logged as they change, but '''only''' when es fires!
   * @param sim the current [[Simulation]]
   * @param obs the observing context implicit
   * @tparan T type of event stream to log
   */
  def log[T](to: LogDestination, es: EventStream[T], sig: Signal[_]*)(implicit sim: Simulation, m: Manifest[T], obs: Observing) {
    // pass an EventStream[List[_]] directly, otherwise map it to one-element List
    val stream = if(m <:< manifest[List[_]]) // manifests against type erasure
      es.asInstanceOf[EventStream[List[_]]].map(_ ::: sig.toList.map(_.now))
    else
      es.map(_ :: sig.toList.map(_.now))
    
    // call dest.log for every change with a list of current values (as long as to is active)
    stream.takeWhile(_ => to.active).foreach(to.log)
  }  

  /**
   * Implicit CanForward to a LogDestination. Can be used to log to a destination using
   * `destination <<: signalOrStream`.
   * @tparam T type of signal/stream
   * @return CanForward to LogDestination for type T
   */
  implicit def forwardLog[T](implicit sim: Simulation, m: Manifest[T]) =
    new CanForward[LogDestination, T] {
      def forward(s: Forwardable[T], dest: => LogDestination)(implicit o: Observing) {
        s match { // match because of overloaded log(...)
          case sig: Signal[_] => log(dest, sig)
          case es: EventStream[_] => log(dest, es.asInstanceOf[EventStream[T]])
        }
      }
    }
}


package logrules {

/**
 * A generatic destination for log values.
 */
trait LogDestination {
  /**
   * Log a list of values.
   * @param values list of values to be logged
   */
  def log(values: List[_]): Unit

  /**
   * Get status of log destination. `false` means it is no longer available
   * and logging to this destination should be stopped.
   */
  def active: Boolean
}

/**
 * A [[LogDestination]] which writes into a file.
 *
 * @param file filename of logfile. Will be created or cleared if needed
 * @param columns (optional) column name list (sets column count), default: "Value"
 * @param timeColumn (optional) column name for simulation time. When set to `null`, time column
 *   will not be logged, default: "Time"
 * @param sep (optional) string used to seperate columns, default: one tab character
 * @param sim the current [[Simulation]]
 */
case class LogFile(file: String, columns: List[String] = List("Value"), timeColumn: String = "Time", header: Boolean = true, sep: String = "\t")(implicit sim: Simulation) extends LogDestination {
  /**
   * PrintWriter for writing to file
   */
  val stream = new PrintWriter(new BufferedWriter(new FileWriter(file)))

  // active until file is closed
  var active = true

  // all used columns
  val allColumns = if(timeColumn != null) (timeColumn :: columns) else columns

  // close file on plugin deactivation
  CoojaTracePlugin.forSim(sim).onCleanUp {
    stream.close()
    active = false
  }

  // add observer to Simulation which flushes log buffers when sim is stopped
  sim.addObserver(new Observer() {
    def update(obs: Observable, obj: Object) {
      if(!sim.isRunning) stream.flush()
    }
  })

  // print header if enabled
  if(header == true) stream println allColumns.mkString(sep)

  def log(values: List[_]) {
    // join values (and time if enabled) with seperator and print 
    val out = if(timeColumn != null) (sim.getSimulationTime :: values) else values
    stream println out.mkString(sep)
  }
}

/**
 * A [[LogDestination]] which writes into a table in a new window.
 *
 * @param name title of window to be created
 * @param columns (optional) column name list (sets column count), default: "Value"
 * @param timeColumn (optional) column name for simulation time. When set to `null`, time column
 *   will not be logged, default: "Time"
 * @param sim the current [[Simulation]]
 */
case class LogWindow(name: String, columns: List[String] = List("Value"), timeColumn: String = "Time")(implicit sim: Simulation) extends LogDestination {
  /**
   * Newly created window (actually an [[InternalFrame]]).
   */
  val window = new JInternalFrame(name, true, true, true, true)

  /**
   * Model to hold table data.
   */
  val model = new javax.swing.table.DefaultTableModel {
    /**
     * Return false for every cell to make entire table read-only.
     */
    override def isCellEditable(row: Int, col: Int) = false
  }

  // add time column if not disabled
  if(timeColumn != null) model.addColumn(timeColumn)

  // add other columns
  columns foreach model.addColumn

  /**
   * Table to display log values
   */
  val table = new JTable(model)
  
  // add (scrollable) table to window and show
  window.add(new JScrollPane(table))
  window.setSize(300, 500)
  window.show()
  sim.getGUI.getDesktopPane.add(window)
  try {
    window.setSelected(true)
  } catch {
    case e: java.beans.PropertyVetoException => // ignore
  }

  // close window on plugin deactivation
  CoojaTracePlugin.forSim(sim).onCleanUp {
    window.dispose()
  }

  def log(values: List[_]) {
    // check for right number of columns
    require(values.size == columns.size, "incorrect column count")

    // create new java vector for row values
    val v = new java.util.Vector[Object](values.length + 1)

    // add timestamp in milliseconds if enabled
    if(timeColumn != null) v.addElement((sim.getSimulationTime/1000.0).asInstanceOf[AnyRef])

    // add value columns
    for(x <- values) v.addElement(x.asInstanceOf[AnyRef])

    // insert entire row
    model.addRow(v)
  }

  // active as long as window is open
  def active = !window.isClosed
}

} // package logrules

} // package rules
