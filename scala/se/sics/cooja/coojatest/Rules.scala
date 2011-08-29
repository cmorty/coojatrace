package se.sics.cooja.coojatest //.Rules


import wrappers._

import reactive._

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import se.sics.cooja._




trait Rule extends Observing



class Assertion(val cond: Signal[Boolean], val sim: Simulation) extends Rule {
  for(c <- cond.distinct.change if c == false) {
    println("ASSERT: " + cond + " is " + c)
    sim.stopSimulation() 
  }
}



class LogDestination(var stream: PrintWriter)
class LogRule(val name: String, val dest: LogDestination, val sim: Simulation, val values: List[Signal[_]]) extends Rule {
  values.tail.foldLeft(values.head.map(s => dest.stream.println(name + "\t" + new RichSimulation(sim).time + "\t" + values.map(_.now).mkString("\t")))) {
    case (combined, signal) => signal.flatMap(s => combined)
  } 
}



object Rules {
  var assertions = List[Assertion]()
  def assert(s:Signal[Boolean])(implicit sim: Simulation) { 
    assertions ::= new Assertion(s, sim)
  }
  
  var logrules = List[LogRule]()
  def log(name: String, s: Signal[_]*)(implicit logdest: LogDestination, sim: Simulation) { 
    logrules ::= new LogRule(name, logdest, sim, s.toList) 
  }
  def logfile(file: String)(implicit logdest: LogDestination) { 
    logdest.stream = new PrintWriter(new BufferedWriter(new FileWriter(file)))
  }
}
