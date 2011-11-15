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
import java.awt.event._

import se.sics.cooja._
import de.fau.cooja.plugins.coojatrace._



/**
 * Script code generator (window).
 *
 * @param plugin CoojaTracePlugin into which code will be inserted
 */
class GeneratorWindow(plugin: CoojaTracePlugin) extends JInternalFrame("Script Generator", true, true, true, true) 
  with DestinationGeneratorComponent with MotesGeneratorComponent
  with OperatorGeneratorComponent with ColumnGeneratorComponent {
  // NOTE: Constructors for all traits have alredy been called when this line is reached

  /**
   * Button panel.
   */
  val buttonPanel = new JPanel()

  /**
   * Generate button.
   */
  val generateButton = new JButton("Generate")
  generateButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      generate()
    }
  })
  buttonPanel.add(generateButton)

  /**
   * Reset button.
   */
  val resetButton = new JButton("Reset")
  resetButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      resetDestination()
      resetMotes()
      resetColumns()
    }
  })
  buttonPanel.add(resetButton)

  // add button panel to window
  add(buttonPanel)

  // set window size
  setSize(400, 500)

  // use BoxLayout to put panels above each other
  getContentPane.setLayout(new BoxLayout(getContentPane, BoxLayout.PAGE_AXIS))

  // pack all elements
  pack()

  // hide this window when closed, do not destroy
  setDefaultCloseOperation(javax.swing.WindowConstants.HIDE_ON_CLOSE)

  // add window to dekstop
  plugin.sim.getGUI.getDesktopPane.add(this)

  /**
   * Get a (name -> value) map from an options panel.
   * @param panel JPanel with JLabels for names and JTextFields for values in alternating order
   * @return Map of (name -> value) pairs
   */
  def readOptions(panel: JPanel): Map[String, String] = {
    val labels = panel.getComponents.collect {
      case lab: JLabel => lab.getText
    }
    val fields = panel.getComponents.collect {
      case tf: JTextField => tf.getText
    }
    (labels zip fields).toMap
  }

  /**
   * Counts generated logdestinations to prevent variable name collisions.
   */
  var counter = 0

  /**
   * Generate the code for specified options and insert in CoojaTrace window.
   */
  private def generate() {
    // create code print/string writer
    val codeWriter = new java.io.StringWriter()
    val code = new java.io.PrintWriter(codeWriter)

    // generate log destination
    val logDest = "logDestination" + counter
    code.println("val " + logDest + " = " + generateDestination())

    // generate mote accessor
    val (moteVal, moteCode, block) = generateMote()
    if(moteCode != "") code.println(moteCode)

    // generate log statement
    if(block) code.print("  ")
    code.print("log(" + logDest)

    val sep = if(block) ",\n    " else ",\n  "
    code.println(generateColumns(moteVal).mkString(sep, sep, ""))

    if(block) code.print("  ")
    code.println(")")

    // close block if neccessary
    if(block) code.println("}")

    // add blank line
    code.println()

    // increase variable name counter
    counter += 1

    // add code to script
    plugin.scriptCode.append(codeWriter.getBuffer.toString)
  
    // focus CoojaTrace window
    try {
      plugin.setSelected(true)
    } catch {
      case e: java.beans.PropertyVetoException => // ignore
    }
  }
}



/**
 * Generator component for log destination options.
 */
trait DestinationGeneratorComponent { this: GeneratorWindow => 
  /**
   * Log destination code generator.
   *
   * @param name name of log destination
   * @param options map of (name -> defaultValue) pairs of options for this generator
   * @param template function taking the options map and a list of column names, outputs code to create log destination
   */
  case class DestinationGenerator(name: String, options: Map[String, String],
    template: (Map[String, String], List[String]) => String) {
    /**
     * Check if this generator can be used.
     * @return `true` if generator is available
     */
    def available = true
  }

  /**
   * All available log destination generators.
   */
  val destinationGenerators = List(
    // Log window destination
    DestinationGenerator("Log window",
      Map("Window title" -> "CoojaTrace Log"),
      (o, c) => "LogWindow(\"" + o("Window title") + "\", " + c.map("\"" + _ + "\"").mkString("List(", ", ", ")") + ", timeColumn = " + o("Time column") + ")"),

    // Log file destination
    DestinationGenerator("Log file",
      Map("Filename" -> "cooja.log", "Header (true/false)" -> "true", "Seperator" -> "\\t"),
      (o, c) => "LogFile(\"" + o("Filename") + "\", " + c.map("\"" + _ + "\"").mkString("List(", ", ", ")") + ", header=" + o("Header (true/false)") + ", sep=\"" + o("Seperator") + "\", timeColumn = " + o("Time column") + ")"),

    // SQLite table destination (if plugin is loaded)
    new DestinationGenerator("SQLite Table",
      Map("SQLite DB file" -> "coojatrace.db", "Table name" -> "log"),
      (o, c) => "sqlitelog.LogTable(sqlitelog.SQLiteDB(\"" + o("SQLite DB file") + "\"), \"" + o("Table name") + "\", " + c.map("\"" + _ + "\"").mkString("List(", ", ", ")") + ", timeColumn = " + o("Time column") + ")") {
      override def available = { // only show if plugin is loaded
        try {
          Class.forName("de.fau.cooja.plugins.coojatrace.rules.logrules.sqlitelog.LogTable")
          true
        } catch {
          case _ => false
        }
      } 
    }
  )

  /**
   * Destination selection panel.
   */
  val destinationPanel = new JPanel(new FlowLayout(FlowLayout.LEADING))
  destinationPanel.setBorder(BorderFactory.createTitledBorder("Destination"))

  /**
   * List of available generators.
   */
  val destGenerators = destinationGenerators.filter(_.available)

  /**
   * Available generator names.
   */
  val destNames = destGenerators.map(_.name)

  /**
   * Destination type combo box.
   */
  val destinationBox = new JComboBox(destNames.toArray.asInstanceOf[Array[Object]])
  destinationPanel.add(destinationBox)

  /**
   * Panel with CardLayout for destination options (panel).
   */
  val destinationOptionsPanel = new JPanel(new CardLayout())
  destinationPanel.add(destinationOptionsPanel)

  /**
   * Map of (destinationName -> OptionPanel) pairs. Used to retrieve correct panel from type selection.
   */
  val destinationOptionPanels = (for(DestinationGenerator(name, options, _) <- destGenerators) yield {
    // create new option panel for this generator
    val panel = new JPanel(new GridLayout(0, 2))
    
    // add option -> value GUI components
    for((option, value) <- options) {
      val label = new JLabel(option)
      val textField = new JTextField(value)
      label.setLabelFor(textField)
      panel.add(label)
      panel.add(textField)
    }

    // add to CardLayout
    destinationOptionsPanel.add(panel, name)

    // return map tuple
    (name, panel)
  }).toMap

  // show correct options panel for selected destination type
  destinationBox.addItemListener(new ItemListener() {
    def itemStateChanged(evt: ItemEvent) {
      val cardLayout = destinationOptionsPanel.getLayout.asInstanceOf[CardLayout]
      cardLayout.show(destinationOptionsPanel, evt.getItem.asInstanceOf[String])
    }
  })

  // add destination selection panel to generator window
  add(destinationPanel)

  /**
   * Generate code for log destination.
   * @return script code for log destination creation
   */
  def generateDestination() = {
    // get selected destination type name
    val destName = destinationBox.getSelectedItem.asInstanceOf[String]
    
    // get column names from table (see ColumnGeneratorComponent)
    val columnNames = columns.map(_._1).toList

    // get time column options (see ColumnGeneratorComponent)
    val timeOptions = ("Time column" -> (if(timeColumnOption.isSelected) "\""+timeColumnName.getText+"\"" else "null"))

    // get destination options and add time column options
    val destOptions = readOptions(destinationOptionPanels(destName)) + timeOptions
    
    // get destination generator by name and call template function with options and column names
    destGenerators.find(_.name == destName).get.template(destOptions, columnNames)
  }

  /**
   * Reset destination options to default values.
   */
  def resetDestination() {
    // select first log destination type
    destinationBox.setSelectedIndex(0)

    // set all option values to defaults
    for { (name, panel) <- destinationOptionPanels
          DestinationGenerator(gname, options, _) <- destinationGenerators
          if(name == gname)
    } panel.getComponents.collect {
      case lab: JLabel => lab.getLabelFor.asInstanceOf[JTextField].setText(options(lab.getText))
    }
  }
}



/**
 * Generator component for mote selection.
 */
trait MotesGeneratorComponent { this: GeneratorWindow => 
  /**
   * Mote selection panel.
   */
  val motePanel = new JPanel(new FlowLayout(FlowLayout.LEADING))
  motePanel.setBorder(BorderFactory.createTitledBorder("Motes"))

  /**
   * Radio button group.
   */
  val moteButtonGroup = new ButtonGroup()

  /**
   * All motes radio button.
   */
  val allMotes = new JRadioButton("All motes")
  allMotes.setSelected(true)
  moteButtonGroup.add(allMotes)
  motePanel.add(allMotes)

  /**
   * Specific motes radio button.
   */
  val specificMotes = new JRadioButton("Specific motes: ")
  moteButtonGroup.add(specificMotes)
  motePanel.add(specificMotes)

  /** 
   * Specific motes text input field.
   */
  val motesInput = new JTextField("2, 3, 5", 20)
  motesInput.setEditable(false)
  motePanel.add(motesInput)

  // change motes input editable property depending on specific mote radio button status 
  allMotes.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      motesInput.setEditable(false)
    }
  })
  specificMotes.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      motesInput.setEditable(true)
      motesInput.requestFocusInWindow() // focus text field
    }
  })

  // add mote selection panel to generator window
  add(motePanel)

  /**
   * Generate code for mote iteration.
   * @return tuple of (mote variable name, loop statement, boolean value if a indented block is created)
   */
  def generateMote() = {
    if(allMotes.isSelected) {
      ("mote", "for(mote <- sim.motes.values) {", true)
    } else {
      val motes = motesInput.getText.split(",").map(_.trim.toInt)
      if(motes.size > 1) {
        ("mote", "for(i <- " +  motes.mkString("List(", ", ", ")") + "; mote = sim.motes(i)) {", true)
      } else {
        ("sim.motes(" + motes.head + ")", "", false)
      }
    }
  }

  /**
   * Reset mote selection options to default values.
   */
  def resetMotes() {
    // select all motes
    allMotes.setSelected(true)
    motesInput.setEditable(false)

    // reset motes input text
    motesInput.setText("2, 3, 5")
  }
}



/**
 * Generator component for operator selection.
 */
trait OperatorGeneratorComponent { this: GeneratorWindow =>
  /**
   * Operator code generator.
   *
   * @param name name of operator
   * @param alwaysEventStream `true` if this operator will return an eventstream even when applied to a signal
   * @param template function taking code for a signal/eventstream, outputs code to apply operator
   */
  case class OperatorGenerator(name: String, alwaysEventStream: Boolean, template: String => String)

  /**
   * List of available operator generators.
   */
  val operatorGenerators = List[OperatorGenerator](
    OperatorGenerator("(No operator)", false, c => c),
    OperatorGenerator("Count", false, "count(" + _ + ")"),
    OperatorGenerator("Maximum", false, "max(" + _ + ")"),
    OperatorGenerator("Minimum", false, "min(" + _ + ")"),
    OperatorGenerator("Average", false, "avg(" + _ + ")"),
    OperatorGenerator("Standard deviation", false, "stdDev(" + _ + ")"),
    OperatorGenerator("Delta", true, "delta(" + _ + ")")
  )

  /**
   * (Default) no-op operator.
   */
  val noOperator = operatorGenerators.head


  /**
   * Operator selection combo box, used as table cell renderer.
   */
  val operatorBox = new JComboBox(operatorGenerators.map(_.name).toArray.asInstanceOf[Array[Object]])
}



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
                    (m, o) => "Val("+ m + ".id)"),
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
                    (m, o) => m + ".moteID.id"),
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
