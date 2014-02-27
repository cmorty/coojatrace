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

package de.fau.cooja.plugins.coojatrace.rules.logrules

import reactive._
import de.fau.cooja.plugins.coojatrace.CoojaTracePlugin

import javax.swing.{JInternalFrame, JTable, JScrollPane}

import org.contikios.cooja.Simulation


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
  sim.getCooja.getDesktopPane.add(window)
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