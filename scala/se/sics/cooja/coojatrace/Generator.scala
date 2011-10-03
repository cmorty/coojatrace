package se.sics.cooja.coojatrace.generator



import javax.swing._
import java.awt.{List => _, _}
import java.awt.event._

import se.sics.cooja._
import coojatrace._


// UGLY GUI CODE BELOW


/**
 * Script code generator.
 */
class GeneratorWindow(plugin: CoojaTracePlugin) extends JInternalFrame("Script Generator", true, true, true, true) {
  /**
   * Destination selection pane.
   */
  val destinationPanel = new JPanel(new FlowLayout(FlowLayout.LEADING))
  destinationPanel.setBorder(BorderFactory.createTitledBorder("Destination"))

  val destGenerators = DestinationGenerators.generators.filter(_.available)
  val destinations = destGenerators.map(_.name).toArray.asInstanceOf[Array[Object]]
  val destinationBox = new JComboBox(destinations)
  destinationPanel.add(destinationBox)

  val destinationOptionsPanel = new JPanel(new CardLayout())
  destinationPanel.add(destinationOptionsPanel)

  val destinationOptionPanels = new collection.mutable.HashMap[String, JPanel]()

  for(DestinationGenerator(name, options, _) <- destGenerators) {
    val panel = new JPanel(new GridLayout(0, 2))
    
    for((option, value) <- options) {
      panel.add(new JLabel(option))
      panel.add(new JTextField(value))
    }

    destinationOptionPanels(name) = panel
    destinationOptionsPanel.add(panel, name)
  }

  destinationBox.addItemListener(new ItemListener() {
    def itemStateChanged(evt: ItemEvent) {
      destinationOptionsPanel.getLayout.asInstanceOf[CardLayout].show(destinationOptionsPanel, evt.getItem.asInstanceOf[String])
    }
  })

	/**
   * Mote selection pane.
   */
  val motePanel = new JPanel(new FlowLayout(FlowLayout.LEADING))
  motePanel.setBorder(BorderFactory.createTitledBorder("Motes"))

  val moteButtonGroup = new ButtonGroup()

  val allMotes = new JRadioButton("All motes")
  allMotes.setSelected(true)
  moteButtonGroup.add(allMotes)
  motePanel.add(allMotes)

  val specificMotes = new JRadioButton("Specific motes: ")
  moteButtonGroup.add(specificMotes)
  motePanel.add(specificMotes)
  specificMotes.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      motesInput.setEditable(specificMotes.isSelected)
    }
  })

  val motesInput = new JTextField("2, 3, 5", 20)
  motesInput.setEditable(false)
  motePanel.add(motesInput)

  /**
   * Column selection pane.
   */
  val columnPanel = new JPanel()
  columnPanel.setLayout(new BoxLayout(columnPanel, BoxLayout.PAGE_AXIS))
  columnPanel.setBorder(BorderFactory.createTitledBorder("Columns"))

  val columns = collection.mutable.ListBuffer[(String, ColumnGenerator, Map[String, String], OperatorGenerator)]()

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
      case 0 => columns(row) = columns(row).copy(_1 = value.asInstanceOf[String])
      case 2 => columns(row) = columns(row).copy(_4 = OperatorGenerators.generators.find(_.name == value.asInstanceOf[String]).get)
    }}
    override def isCellEditable(row: Int, col: Int) = (col != 1) 
  }

  val timeColumnPanel = new JPanel(new FlowLayout(FlowLayout.LEADING))
  columnPanel.add(timeColumnPanel)
  val timeColumnOption = new JCheckBox("Simulation time (ns) column, column name:")
  timeColumnOption.setSelected(true)
  timeColumnPanel.add(timeColumnOption)
  val timeColumnName = new JTextField("Time", 10)
  timeColumnPanel.add(timeColumnName)
  timeColumnOption.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      timeColumnName.setEditable(timeColumnOption.isSelected)
    }
  })


  val operators = OperatorGenerators.generators.map(_.name).toArray.asInstanceOf[Array[Object]]
  val operatorBox = new JComboBox(operators)


  val columnTable = new JTable(columnModel)
  columnTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  columnTable.setRowSelectionAllowed(true)
  columnTable.setColumnSelectionAllowed(false)
  columnTable.setFillsViewportHeight(true)
  columnTable.getColumnModel.getColumn(2).setCellEditor(new DefaultCellEditor(operatorBox))
  columnPanel.add(new JScrollPane(columnTable))

  val editPanel = new JPanel(new FlowLayout(FlowLayout.LEADING))
  columnPanel.add(editPanel)

  val columnTypes = ColumnGenerators.generators.map(_.name).toArray.asInstanceOf[Array[Object]]
  val columnType = new JComboBox(columnTypes)
  editPanel.add(columnType)
  columnType.addItemListener(new ItemListener() {
    def itemStateChanged(evt: ItemEvent) {
      columnOptionsPanel.getLayout.asInstanceOf[CardLayout].show(columnOptionsPanel, evt.getItem.asInstanceOf[String])
    }
  })

  val addButton = new JButton("add")
  addButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      val colType = ColumnGenerators.generators.find(_.name == columnType.getSelectedItem.asInstanceOf[String]).get
      val options = readOptions(columnOptionPanels(colType.name))
      val newCol = (colType.name, colType, options, OperatorGenerators.none)
      if(colType.eventStream) {
        if(!columns.isEmpty && (columns.head._2.eventStream || columns.head._4.alwaysEventStream))
          return // TODO: ERROR
        columns.prepend(newCol)
        columnModel.fireTableRowsInserted(0, 0)
      } else {
        columns.append(newCol)
        columnModel.fireTableRowsInserted(columnModel.getRowCount-1, columnModel.getRowCount-1)
      }
    }
  })
  editPanel.add(addButton)

  val removeButton = new JButton("remove")
  removeButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      for(r <- columnTable.getSelectedRows) {
        columnModel.fireTableRowsDeleted(r, r)
        columns.remove(r)
      }
    }
  })
  editPanel.add(removeButton)

  val columnOptionsPanel = new JPanel(new CardLayout())
  columnPanel.add(columnOptionsPanel)

  val columnOptionPanels = new collection.mutable.HashMap[String, JPanel]()

  for(ColumnGenerator(name, _, options, _) <- ColumnGenerators.generators) {
    val panel = new JPanel(new GridLayout(0, 2))
    
    for(option <- options) {
      panel.add(new JLabel(option))
      panel.add(new JTextField())
    }

    columnOptionPanels(name) = panel
    columnOptionsPanel.add(panel, name)
  }


  /**
   * Button pane.
   */
  val buttonPanel = new JPanel()

  /**
   * Insert button.
   */
  val insertButton = new JButton("Generate")
  insertButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      generate()
    }
  })
  buttonPanel.add(insertButton)


  /**
   * Reset button.
   */
  val resetButton = new JButton("Reset")
  insertButton.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      // TODO
    }
  })
  buttonPanel.add(resetButton)

  add(destinationPanel)
  add(motePanel)
  add(columnPanel)
  add(buttonPanel)

  setSize(400, 500)
  getContentPane.setLayout(new BoxLayout(getContentPane, BoxLayout.PAGE_AXIS))
  pack()
  setDefaultCloseOperation(javax.swing.WindowConstants.HIDE_ON_CLOSE)
  plugin.sim.getGUI.getDesktopPane.add(this)

  private def readOptions(panel: JPanel): Map[String, String] = {
    val labels = panel.getComponents.collect {
      case lab: JLabel => lab.getText
    }
    val fields = panel.getComponents.collect {
      case tf: JTextField => tf.getText
    }
    (labels zip fields).toMap
  }

  var counter = 0

  private def generate() {
    var code = "" 

    val columnNames = columns.map(_._1).toList
    val destName = destinationBox.getSelectedItem.asInstanceOf[String]
    val timeOptions = ("Time column" -> (if(timeColumnOption.isSelected) "\""+timeColumnName.getText+"\"" else "null"))
    val destOptions = readOptions(destinationOptionPanels(destName)) + timeOptions
    code += "val logDestination" + counter + " = "
    code += destGenerators.find(_.name == destName).get.template(destOptions, columnNames)
    code += "\n"

    var moteVal = "mote"
    if(allMotes.isSelected) {
      code += "for(mote <- sim.motes.values) {\n"
    } else {
      val motes = motesInput.getText.split(",").map(_.trim.toInt)
      if(motes.size > 1) {
        code += "for(i <- " +  motes.mkString("List(", ", ", ")") + "; mote = sim.motes(i)) {\n"
      } else {
        moteVal = "sim.motes(" + motes.head + ")"
      }
    }

    if(moteVal == "mote") code += "  "
    code += "log(logDestination" + counter

    for((name, col, colOpts, operator) <- columns) {
      code += ",\n    "
      if(moteVal == "mote") code += "  "

      code += operator.template(col.template(moteVal, colOpts))
    }

    code += "\n"
    if(moteVal == "mote") code += "  "
    code += ")\n"

    if(moteVal == "mote") code += "}\n"

    code += "\n"

    counter += 1

    plugin.scriptCode.append(code)
    try {
      plugin.setSelected(true)
    } catch {
      case e: java.beans.PropertyVetoException => // ignore
    }
  }
}


case class DestinationGenerator(name: String, options: Map[String, String],
                                template: (Map[String, String], List[String]) => String) {
  def available = true
}

object DestinationGenerators {
  val generators = List(
    DestinationGenerator("Log file",
      Map("Filename" -> "cooja.log", "Header (true/false)" -> "true", "Seperator" -> "\\t"),
      (o, c) => "LogFile(\"" + o("Filename") + "\", " + c.map("\"" + _ + "\"").mkString("List(", ", ", ")") + ", header=" + o("Header (true/false)") + ", sep=\"" + o("Seperator") + "\", timeColumn = " + o("Time column") + ")"),

    DestinationGenerator("Log window",
      Map("Window title" -> "CoojaTrace Log"),
      (o, c) => "LogWindow(\"" + o("Window title") + "\", " + c.map("\"" + _ + "\"").mkString("List(", ", ", ")") + ", timeColumn = " + o("Time column") + ")"),

    new DestinationGenerator("SQLite Table",
      Map("SQLite DB file" -> "coojatrace.db", "Table name" -> "log"),
      (o, c) => "LogTable(SQLiteDB(\"" + o("SQLite DB file") + "\"), \"" + o("Table name") + "\", " + c.map("\"" + _ + "\"").mkString("List(", ", ", ")") + ", timeColumn = " + o("Time column") + ")") {
      override def available = { // only show if plugin is loaded
        try {
          Class.forName("se.sics.cooja.coojatrace.rules.logrules.sqlitelog.LogTable")
          true
        } catch {
          case _ => false
        }
      } 
    }
  )
}


case class OperatorGenerator(name: String, alwaysEventStream: Boolean, template: String => String)


object OperatorGenerators {
  val none = OperatorGenerator("(No operator)", false, c => c)
  val generators = List[OperatorGenerator](
    none,
    OperatorGenerator("Count", false, "count(" + _ + ")"),
    OperatorGenerator("Maximum", false, "max(" + _ + ")"),
    OperatorGenerator("Minimum", false, "min(" + _ + ")"),
    OperatorGenerator("Average", false, "avg(" + _ + ")"),
    OperatorGenerator("Standard deviation", false, "stdDev(" + _ + ")"),
    OperatorGenerator("Delta", true, "delta(" + _ + ")")
  )
}


case class ColumnGenerator(name: String, eventStream: Boolean, options: List[String],
                           template: (String, Map[String, String]) => String)

object ColumnGenerators {
  val generators = List[ColumnGenerator](
    ColumnGenerator("Mote Name", false, Nil,
                    (m, o) => "Val("+ m + ".toString)"),
    ColumnGenerator("Mote ID (static)", false, Nil,
                    (m, o) => m + ".id"),
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
    ColumnGenerator("Watchpoint", false, List("Source filename", "Line number", "Name"),
                    (m, o) => m + ".watchpoint(\"" + o("Source filename") + "\", \"" + o("Line number") + "\", \"" + o("Name") + "\")"),
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
}
