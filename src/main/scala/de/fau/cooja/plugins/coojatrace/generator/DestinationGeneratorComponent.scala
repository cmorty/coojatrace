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
          case _:Throwable => false
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

