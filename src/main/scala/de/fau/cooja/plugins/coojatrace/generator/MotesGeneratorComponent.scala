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
      ("mote", "for(mote <- sim.allMotes) {", true)
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

