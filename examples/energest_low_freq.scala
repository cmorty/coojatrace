//Set sampling frequency
val freq = 1000  //1s

//Extra time coloumn is need. Automatic timestamping can be turned off to rece data
val dest_energy =  LogFile("energest.trace" , List("time", "mote", "energy" ), timeColumn=null)

// constants for sky motes
val voltage = 3 // V
val receiveConsumption = 20.0 * voltage // mW
val transmitConsumption = 17.7 * voltage // mW
val activeConsumption = 1.800 * voltage // mW
val idleConsumption = 0.0545 * voltage // mW

for(mote <- sim.allMotes) {
  val receiveTime = timeSum(mote.radio.receiverOn) 
  val transmitTime = timeSum(mote.radio.transmitting)
  val activeTime = timeSum(mote.cpuMode === "active")
  val idleTime = timeSum(mote.cpuMode =!= "active")

  val energy: Signal[Double] = receiveTime / 1E6 * receiveConsumption + 
    transmitTime / 1E6 * transmitConsumption +
    activeTime / 1E6 * activeConsumption +
    idleTime / 1E6 * idleConsumption 

  //If the second parameter is a event stream the signals are sampled when it fires 
  log(dest_energy, sim.milliSeconds.change.filter(_ % freq == 0) , mote, energy)
}
