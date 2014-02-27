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
import java.awt.event.{ActionListener, ActionEvent, ItemListener, ItemEvent}
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
  plugin.sim.getCooja.getDesktopPane.add(this)

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
    plugin.scriptCode.setText(plugin.scriptCode.getText + codeWriter.getBuffer.toString)
  
    // focus CoojaTrace window
    try {
      plugin.setSelected(true)
    } catch {
      case e: java.beans.PropertyVetoException => // ignore
    }
  }
}

