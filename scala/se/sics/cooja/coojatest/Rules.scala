package se.sics.cooja.coojatest



import wrappers._

import reactive._

import javax.swing.{JInternalFrame, JTable, JScrollPane}
import java.io.{BufferedWriter, FileWriter, PrintWriter, Writer}
import java.util.{Observer, Observable}

import se.sics.cooja._



/**
 * Package for rules, which are (output) sinks for events and signals.
 */
package object rules {
  /**
   * List of all active [[Assertion]]s.
   */
  private var assertions = List[Assertion]()

  /**
   * Create new assertion from (boolean) [[Signal]].
   * Assertion is raised when signal changes to `false`.
   * @param sig [[Signal]]`[Boolean]` to check
   * @param name name of assertion. Will be printed when raised
   * @param sim the current [[Simulation]]
   */
  def assert(sig: Signal[Boolean], name: String)(implicit sim: Simulation) { 
    assertions ::= new Assertion(sig, name, sim)
  }

  /**
   * Create new assertion from (boolean) [[EventStream]].
   * Assertion is raised `false` is received.
   * @param es [[EventStream]]`[Boolean]` to check
   * @param name name of assertion. Will be printed when raised
   * @param sim the current [[Simulation]]
   */
  def assert(es: EventStream[Boolean], name: String)(implicit sim: Simulation) { 
    // convert stream to signal and call assert for signals
    assert(es.hold(true), name)(sim)
  }
  


  /**
   * List of all active [[LogRule]]s.
   */
  private var logrules = List[LogRule]()

  /**
   * Create new rule to log one or more signals.
   * @param to [[LogDestination]]-object where values shall be logged to
   * @param sig [[Signal]]s to be logged. If multiple signals are given, a change in any 
   * of the signals will generate a new log-line listing ´´´all´´´ current values
   * @param sim the current [[Simulation]]
   */
  def log(to: LogDestination, sig: Signal[_]*)(implicit sim: Simulation) { 
    logrules ::= new LogRule(to, sim, sig.toList) 
  }

  /**
   * Create new rule to log an eventstream.
   * @param to [[LogDestination]]-object where values shall be logged to
   * @param es [[EventStream]] to be logged.
   * @param sim the current [[Simulation]]
   */
  def log(to: LogDestination, es: EventStream[_])(implicit sim: Simulation) { 
    logrules ::= new LogRule(to, sim, es) 
  }

  /**
   * Resets all active rules.
   */
  def reset() {
    assertions = Nil
    logrules = Nil
  }
}

package rules {
  /**
   * Represents a rule for testing.
   */
  trait Rule extends Observing



  /**
   * A rule that stops simulation and outputs value and name when input signal
   * changes to `false`.
   */ 
  class Assertion(val cond: Signal[Boolean], val name: String, val sim: Simulation) extends Rule {
    // only check on actual changes
    for(c <- cond.distinct) {
      // assertion violated
      if(c == false) {
        // error and stop (TODO: use logger?)
        println("ASSERT: " + (if(name != null) name else cond) + " is " + c)
        sim.stopSimulation() 
      }
    }
  }


  /**
   * A generatic destination for log values.
   */
  trait LogDestination {
    /**
     * Log a timestamped list of values.
     * @param time simulated time in microseconds
     * @param values list of values to be logged
     */
    def log(time: Long, values: List[_]): Unit
  }

  /**
   * A [[LogDestination]] which writes into a file.
   *
   * @param file filename of logfile. Will be created or cleared if needed
   * @param sep string used to seperate columns, default is one tab character
   * @param sim the current [[Simulation]]
   */
  case class LogFile(file: String, sep: String = "\t")(implicit sim: Simulation) extends LogDestination {
    /**
     * PrintWriter for writing to file
     */
    val stream = new PrintWriter(new BufferedWriter(new FileWriter(file)))

    // add observer to [[Simulation]] which flushes log buffers when sim is stopped
    sim.addObserver(new Observer() {
      def update(obs: Observable, obj: Object) {
        if(!sim.isRunning) stream.flush()
      }
    })

    def log(time: Long, values: List[_]) {
      // join time and values with seperator and print
      stream println (time :: values).mkString(sep)
    }
  }

  /**
   * A [[LogDestination]] which writes into a table in a new window.
   *
   * @param name title of window to be created
   * @param valueNames (optional) header names for value columns
   * @param sim the current [[Simulation]]
   */
  case class LogWindow(name: String, valueNames: String*)(implicit sim: Simulation) extends LogDestination {
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

    // add columns
    model.addColumn("Time")
    if(valueNames.isEmpty)
      model.addColumn("Value")
    else
      valueNames foreach model.addColumn

    /**
     * Table to display log values
     */
    val table = new JTable(model)
    
    // add (scrollable) table to window and show
    window.add(new JScrollPane(table))
    window.setSize(300, 500)
    window.show()
    sim.getGUI.getDesktopPane.add(window)

    def log(time: Long, values: List[_]) {
      // create new java vector for row values
      val v = new java.util.Vector[Object](values.length + 1)

      // add timestamp in milliseconds
      v.addElement((time/1000.0).asInstanceOf[AnyRef])

      // add value columns
      for(x <- values) v.addElement(x.asInstanceOf[AnyRef])

      // insert entire row
      model.addRow(v)
    }
  }

  /**
   * A rule that logs an eventstream or signals to a [[LogDestination]].
   * @param dest the [[LogDestination]] to log to
   * @param sim the current [[Simulation]]
   * @param es an [[EventStream]] which values are logged
   */
  class LogRule(val dest: LogDestination, val sim: Simulation, val es: EventStream[_]) extends Rule {
    /**
     * Alternate constructor to log one or more signals.
     * @param dest the [[LogDestination]] to log to
     * @param sim the current [[Simulation]]
     * @param values one or more [[Signal]]s which changes are logged
     */
    def this(dest: LogDestination, sim: Simulation, values: List[Signal[_]]) = this(
      dest, sim, values.tail.foldLeft(values.head) {
        case (combined, signal) => signal.flatMap(s => combined)
      }.map {
        x => values.map(_.now.toString)
      }.change
    )

    // call dest.log for every change with a list of current values
    for(e <- es) dest.log(new RichSimulation(sim).time, e match {
      case list: List[_] => list
      case event: Any => List(event) // turn single value into list
    })
  }
}
