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


package de.fau.cooja.plugins.coojatrace.generator



import javax.swing._
import java.awt.{List => _, _}
import java.awt.event.{ActionListener, ActionEvent, ItemListener, ItemEvent}

import se.sics.cooja.{GUI}


/**
 * Generator component for column selection.
 */
trait ColumnGeneratorComponent { this: GeneratorWindow =>
  /**
   * Log column code generator.
   *
   * @param name name of column value type
   * @param eventStream `true` if this generator return an event stream
   * @param options list of option names
   * @param template function taking mote accessor code and options map, outputs code to apply operator
   */
  case class ColumnGenerator(name: String, eventStream: Boolean, options: List[String],
    template: (String, Map[String, String]) => String)

  /**
   * List of available column generators.
   */
  val columnGenerators = List[ColumnGenerator](
    ColumnGenerator("Mote Name", false, Nil,
                    (m, o) => "Val("+ m + ".toString)"),
    ColumnGenerator("Mote ID (static)", false, Nil,
                    (m, o) => "Val("+ m + ".getID)"),
    ColumnGenerator("LED status", false, Nil,
                    (m, o) => m + ".led.status"),
    ColumnGenerator("Radio events", true, Nil,
                    (m, o) => m + ".radio.events"),
    ColumnGenerator("Radio interference status", false, Nil,
                    (m, o) => m + ".radio.interfered"),
    ColumnGenerator("Radio receiver status", false, Nil,
                    (m, o) => m + ".radio.receiverOn"),
    ColumnGenerator("Radio reception status", false, Nil,
                    (m, o) => m + ".radio.receiving"),
    ColumnGenerator("Radio transmission status", false, Nil,
                    (m, o) => m + ".radio.transmitting"),
    ColumnGenerator("Radio channel", false, Nil,
                    (m, o) => m + ".radio.channel"),
    ColumnGenerator("Radio output power", false, Nil,
                    (m, o) => m + ".radio.currentOutputPower"),
    ColumnGenerator("Radio output power indicator", false, Nil,
                    (m, o) => m + ".radio.currentOutputPowerIndicator"),
    ColumnGenerator("Radio signal strength", false, Nil,
                    (m, o) => m + ".radio.currentSignalStrength"),
    ColumnGenerator("Radio position", false, Nil,
                    (m, o) => m + ".radio.position"),
    ColumnGenerator("Radio transmitted packet data (hex)", true, Nil,
                    (m, o) => m + ".radio.packetsTransmitted.map(_.getPacketData.map(\"%02X\".format(_)).mkString)"),
    ColumnGenerator("Radio received packet data (hex)", true, Nil,
                    (m, o) => m + ".radio.packetsReceived.map(_.getPacketData.map(\"%02X\".format(_)).mkString)"),
    ColumnGenerator("Radio transmissions", true, Nil,
                    (m, o) => m + ".radio.transmissions"),
    ColumnGenerator("Radio receptions", true, Nil,
                    (m, o) => m + ".radio.receptions"),
    ColumnGenerator("Log messages", true, Nil,
                    (m, o) => m + ".log.messages"),
    ColumnGenerator("Beeper status", false, Nil,
                    (m, o) => m + ".beeper.beeping"),
    ColumnGenerator("Button status", false, Nil,
                    (m, o) => m + ".button.pressed"),
    ColumnGenerator("IP address", false, Nil,
                    (m, o) => m + ".ipAddress.ipAddress"),
    ColumnGenerator("Rime address", false, Nil,
                    (m, o) => m + ".rimeAddress.address"),
    ColumnGenerator("Mote ID (dynamic)", false, Nil,
                    (m, o) => m + ".getID"),
    ColumnGenerator("Mote position", false, Nil,
                    (m, o) => m + ".position.position"),  
    ColumnGenerator("Mote attributes", false, Nil,
                    (m, o) => m + ".moteAttributes.attributes"),
    ColumnGenerator("Contiki process name (from .map file)", false, Nil,
                    (m, o) => m + ".currentProcess.name"),
    ColumnGenerator("Contiki process name (from memory)", false, Nil,
                    (m, o) => m + ".currentProcessDynamic.name"),
    ColumnGenerator("Contiki process address", false, Nil,
                    (m, o) => m + ".currentProcess.address"),
    ColumnGenerator("Stack Pointer", false, Nil,
                    (m, o) => m + ".cpu.stackPointer"),
    ColumnGenerator("Register", false, List("Register Name"),
                    (m, o) => m + ".cpu.register(\"" + o("Register Name") + "\")"),
    ColumnGenerator("Watchpoint", true, List("Source filename", "Line number", "Name"),
                    (m, o) => m + ".watchpoint(\"" + o("Source filename") + "\", " + o("Line number") + ", \"" + o("Name") + "\")"),
    ColumnGenerator("Memory variable (byte/char)", false, List("Name", "or Address"),
                    (m, o) => m + ".memory.variable(" + ( if(o("Name") != "") "\""+o("Name")+"\"" else o("or Address") ) + ", CByte)"),
    ColumnGenerator("Memory variable (int)", false, List("Name", "or Address"),
                    (m, o) => m + ".memory.variable(" + ( if(o("Name") != "") "\""+o("Name")+"\"" else o("or Address") ) + ", CInt)"),
    ColumnGenerator("Memory variable (byte array)", false, List("Name", "or Address", "Length"),
                    (m, o) => m + ".memory.variable(" + ( if(o("Name") != "") "\""+o("Name")+"\"" else o("or Address") ) + ", CArray(" + o("Length") + "))"),
    ColumnGenerator("(SIMULATION) Mote Relations", false, Nil,
                    (m, o) => "sim.moteRelations.relations"),
    ColumnGenerator("(SIMULATION) Log", true, Nil,
                    (m, o) => "sim.log.messages"),
    ColumnGenerator("(SIMULATION) Milliseconds", false, Nil,
                    (m, o) => "sim.milliSeconds"),
    ColumnGenerator("(RADIOMEDIUM) Transmissions", true, Nil,
                    (m, o) => "sim.radioMedium.transmissions"),
    ColumnGenerator("(RADIOMEDIUM) Connections", false, Nil,
                    (m, o) => "sim.radioMedium.connections"),
    ColumnGenerator("Constant string", false, List("String"),
                    (m, o) => "Val(\"" + o("String") + "\")")
  )


  /**
   * List of created columns. Contains (columnName, columnGenerator, columnOptions, operatorGenerator) tuples
   */  
  val columns = collection.mutable.ListBuffer[(String, ColumnGenerator, Map[String, String], OperatorGenerator)]()

  /**
   * Data model for column table.
   */
  val columnModel = new javax.swing.table.AbstractTableModel {
    override def getColumnName(col: Int): String = List("Column name", "Source", "Operator")(col)
    def getRowCount = columns.size
    def getColumnCount = 3
    def getValueAt(row: Int, col: Int): Object = col match {
      case 0 => columns(row)._1
      case 1 => columns(row)._2.name
      case 2 => columns(row)._4.name
    }
    override def setValueAt(value: Object, row: Int, col: Int) { col match {
      case 0 =>
        columns(row) = columns(row).copy(_1 = value.asInstanceOf[String])
      case 2 => {
        val operator = operatorGenerators.find(_.name == value.asInstanceOf[String]).get
        if(operator.alwaysEventStream && (columns(row) != columns.head) && (columns.head._2.eventStream || columns.head._4.alwaysEventStream)) {
          // more than one eventstream cannot be logged, show error and do nothing
          JOptionPane.showMessageDialog(GUI.getTopParentContainer,
            "You cannot use more than one event stream in a log rule.\n\n" +
            "Using this operator turns this column into an event stream, which is already present.", 
            "Event stream conflict", JOptionPane.WARNING_MESSAGE) 
        } else {
          columns(row) = columns(row).copy(_4 = operator)
          if(operator.alwaysEventStream) {
            columns.prepend(columns(row))
            columns.remove(row+1)
            fireTableRowsDeleted(row, row)
            fireTableRowsInserted(0, 0)
          } 
        }
      }
    }}
    override def isCellEditable(row: Int, col: Int) = (col != 1) 
  }


  /**
   * Column selection panel.
   */
  val columnPanel = new JPanel()
  columnPanel.setLayout(new BoxLayout(columnPanel, BoxLayout.PAGE_AXIS))
  columnPanel.setBorder(BorderFactory.createTitledBorder("Columns"))


  /**
   * Time column options panel.
   */  
  val timeColumnPanel = new JPanel(new FlowLayout(FlowLayout.LEADING))
  columnPanel.add(timeColumnPanel)

  /**
   * Time column check box.
   */
  val timeColumnOption = new JCheckBox("Simulation time (ns) column, column name:")
  timeColumnOption.setSelected(true)
  timeColumnPanel.add(timeColumnOption)

  /**
   * Time column name text field.
   */
  val timeColumnName = new JTextField("Time", 10)
  timeColumnPanel.add(timeColumnName)

  // enable/disable time column name text field based on time column check box
  timeColumnOption.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      timeColumnName.setEditable(timeColumnOption.isSelected)
    }
  })


  /**
   * Columns table.
   */
  val columnTable = new JTable(columnModel)
  columnTable.setFillsViewportHeight(true)
  columnPanel.add(new JScrollPane(columnTable))

  // only allow single row selection (confuses remove otherwise)
  columnTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  columnTable.setRowSelectionAllowed(true)
  columnTable.setColumnSelectionAllowed(false)

  // use operator selection combo box for operator column cells
  columnTable.getColumnModel.getColumn(2).setCellEditor(new DefaultCellEditor(operatorBox))


  /**
   * Column add/remove panel.
   */
  val editPanel = new JPanel(new FlowLayout(FlowLayout.LEADING))
  columnPanel.add(editPanel)

  /**
   * Column type selection combo box.
   */
  val columnType = new JComboBox(columnGenerators.map(_.name).toArray.asInstanceOf[Array[Object]])
  editPanel.add(columnType)

  // show correct options panel for selected destination type
  columnType.addItemListener(new ItemListener() {
    def itemStateChanged(evt: ItemEvent) {
      val cardLayout = columnOptionsPanel.getLayout.asInstanceOf[CardLayout]
      cardLayout.show(columnOptionsPanel, evt.getItem.asInstanceOf[String])
    }
  })

  /**
   * Column add button.
   */
  val addButton = new JButton("add")
  addButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      val colType = columnGenerators.find(_.name == columnType.getSelectedItem.asInstanceOf[String]).get
      val options = readOptions(columnOptionPanels(colType.name))
      val newCol = (colType.name, colType, options, noOperator)
      if(colType.eventStream) {
        if(!columns.isEmpty && (columns.head._2.eventStream || columns.head._4.alwaysEventStream)) {
          // more than one eventstream cannot be logged, show error and do nothing
          JOptionPane.showMessageDialog(GUI.getTopParentContainer,
            "You cannot use more than one event stream in a log rule.\n",
            "Event stream conflict", JOptionPane.WARNING_MESSAGE) 
          return
        }
        columns.prepend(newCol)
        columnModel.fireTableRowsInserted(0, 0)
      } else {
        columns.append(newCol)
        columnModel.fireTableRowsInserted(columnModel.getRowCount-1, columnModel.getRowCount-1)
      }
    }
  })
  editPanel.add(addButton)

  /** 
   * Column remove button.
   */
  val removeButton = new JButton("remove")
  removeButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      for(r <- columnTable.getSelectedRows) {
        columns.remove(r)
        columnModel.fireTableRowsDeleted(r, r)
      }
    }
  })
  editPanel.add(removeButton)

  /**
   * Column options card layout.
   */
  val columnOptionsPanel = new JPanel(new CardLayout())
  columnPanel.add(columnOptionsPanel)

  /**
   * Map of (columnName -> OptionPanel) pairs. Used to retrieve correct panel from type selection.
   */
  val columnOptionPanels = (for(ColumnGenerator(name, _, options, _) <- columnGenerators) yield {
    val panel = new JPanel(new GridLayout(0, 2))
    
    for(option <- options) {
      val label = new JLabel(option)
      val textField = new JTextField()
      label.setLabelFor(textField)
      panel.add(label)
      panel.add(textField)
    }

    columnOptionsPanel.add(panel, name)
    (name, panel)
  }).toMap

  // add column selection panel to generator window
  add(columnPanel)

  /**
   * Generate code for log columns.
   * @return list of script code string for log columns
   */
  def generateColumns(moteVal: String) =
    for((_, col, colOpts, operator) <- columns) yield operator.template(col.template(moteVal, colOpts))

  /**
   * Reset column selection to default values.
   */
  def resetColumns() {
    // clear column list, update table
    columns.clear()
    columnModel.fireTableDataChanged()

    // reset time column options
    timeColumnOption.setSelected(true)
    timeColumnName.setText("Time")
    timeColumnName.setEditable(false)

    // reset all column options to default values
    columnType.setSelectedIndex(0)

    // clear all option values
    for { (name, panel) <- columnOptionPanels } panel.getComponents.collect {
      case tf: JTextField => tf.setText("")
    }
  }
}

