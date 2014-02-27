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

package de.fau.cooja.plugins.coojatrace.rules


import reactive._
import de.fau.cooja.plugins.coojatrace._
import org.contikios.cooja.{Simulation, Cooja}

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

        if(Cooja.isVisualized) {
          // show dialog if visualized
          javax.swing.SwingUtilities.invokeLater(new Runnable() {
            def run() {
              javax.swing.JOptionPane.showMessageDialog(Cooja.getTopParentContainer,
                "The following assertion failed and stopped the simulation:\n\n" + name,
                "Assertion failed", javax.swing.JOptionPane.INFORMATION_MESSAGE)
            }
          })          
        } else {
          // quit cooja if not visualized (code from LogScriptEngine plugin)
          new Thread() {
            override def run() {
              try { Thread.sleep(500) } catch { case e: InterruptedException => }
              sim.getCooja.doQuit(false)
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
