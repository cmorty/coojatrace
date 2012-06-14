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


