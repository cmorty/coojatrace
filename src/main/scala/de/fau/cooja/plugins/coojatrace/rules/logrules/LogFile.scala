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



import de.fau.cooja.plugins.coojatrace._

import reactive._

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.util.{Observer, Observable}
import java.util.concurrent.Semaphore

import org.contikios.cooja.Simulation

/**
 * A [[LogDestination]] which writes into a file.
 *
 * @param file filename of logfile. Will be created or cleared if needed
 * @param columns (optional) column name list (sets column count), default: "Value"
 * @param timeColumn (optional) column name for simulation time. When set to `null`, time column
 *   will not be logged, default: "Time"
 * @param sep (optional) string used to seperate columns, default: one tab character
 * @param sim the current [[Simulation]]
 */
case class LogFile(file: String, columns: List[String] = List("Value"), timeColumn: String = "Time", header: Boolean = true, sep: String = "\t")(implicit sim: Simulation) extends LogDestination {
  /**
   * Logger.
   */
  val logger = org.apache.log4j.Logger.getLogger(this.getClass)
  
  
  /**
   * A Semaphore to limit the message queue
   */
  val sem = new Semaphore(100) 
  
  
  /**
   * PrintWriter for writing to file
   */
  val stream = new PrintWriter(new BufferedWriter(new FileWriter(file)))

  var active = true
  

  // all used columns
  val allColumns = if(timeColumn != null) (timeColumn :: columns) else columns

  // close file on plugin deactivation
  case class Shutdown()
  
  CoojaTracePlugin.forSim(sim).onCleanUp {
    active = false
    writer ! new Shutdown    
  }
  
  

    //Use actor to write to file
  import scala.actors._
  object writer extends Actor {
    def act() {
      var shutdown = false
      //We need to react, while we are waiting, but shutdown
      //when done.
      loopWhile(!shutdown){
        react {
          case str:String => {
            stream println str
            sem.release()
          }
          case sd:Shutdown => {shutdown = false}
          case _ => {logger.error("Something went wrong here!")}
        }
      }
      logger.info("Closing " + file);
      stream.close()
    }
   
  }
  
  writer.start()
  
  // add observer to Simulation which flushes log buffers when sim is stopped
  sim.addObserver(new Observer() {
    def update(obs: Observable, obj: Object) {
      if(!sim.isRunning) stream.flush()
    }
  })

  // print header if enabled
  if(header == true) stream println allColumns.mkString(sep)

  def log(values: List[_]) {
    // join values (and time if enabled) with seperator and print 
    val out = if(timeColumn != null) (sim.getSimulationTime :: values) else values
    if (!sem.tryAcquire()){
	    logger.warn("The Harddisk is slowing us down")
	    sem.acquire() //Make sure we slow things down until we get a semaphore
	}
    writer ! out.mkString(sep)
  }
}
