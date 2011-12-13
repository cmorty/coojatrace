val dest_stats = LogFile("collect.trace", List( "mote", "var", "value"))

val members = List("foundroute", "newparent", "routelost", "acksent", 
 "datasent", "datarecv", "ackrecv", "badack", "duprecv", "qdrop", 
 "rtdrop", "ttldrop", "ackdrop", "timedout")
var size = 4 // bytes

def array2Int(arr: Array[Byte]):Int = arr.foldRight(0) { (byte, sum) => (sum << 8) + (byte & 0xFF) }

for(mote <- sim.allMotes) {
  val stats = mote.memory.variable("stats", CArray(size))
  for(s <- members) {
    var index = members.indexOf(s)
    log(dest_stats, mote, s, *(&(stats) + index).map(array2Int))
  }
}

