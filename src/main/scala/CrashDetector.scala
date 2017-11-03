import java.util.NoSuchElementException

import scala.collection.mutable.ArrayBuffer
import scala.xml._

// class for error objects
case class configException(text: String) extends Exception(text)

object static_error {
  def report(msg: String) = {
    throw new configException("Error: " + msg)
  }
}

// class for crash objects
class Crash(val station: Int, val time: Int, val engines: ArrayBuffer[Int])

// railway network
class RailNet (val StatNum: Int, val EngineNum: Int ) {

    // branches(si) represents the table of outgoing branches from station si
    // in other words branches(from)(to) is the length of from->to branch
    val  branches = new Array[scala.collection.mutable.Map[Int, Int]](StatNum)

    // routes(ei) represents the route (array of stations) of engine ei
    val  routes = new Array[ArrayBuffer[Int]](EngineNum)

    // load this object from a xml object
    def loadFromXML(xml: scala.xml.Elem) : Unit = {
        for(child<-xml.child) {
            // load from <branch> nodes
            if (child.label == "branch") {
                // a <branch> node like <branch From="0" To="2" Length="1"/>
                val From = child.attribute("From").get(0).text.toInt
                val To = child.attribute("To").get(0).text.toInt
                val Length = child.attribute("Length").get(0).text.toInt

                if (From < 0 || From >= StatNum || To < 0 || To >= StatNum || Length <= 0)
                    // a invalid branch attribute
                     static_error.report("Incorrect XML node: " + child)
                var out_from = branches(From) // table of branches outgoing from the From
                if (out_from == null)
                    out_from = new scala.collection.mutable.HashMap[Int, Int]()
                if (out_from.getOrElse(To, Length) != Length)
                    // an ambiguously duplicated branch
                    static_error.report("The Length of branch " + From + "->" + To + " is ambiguously defined: " +
                                      out_from(To) + " vs " + Length)
                // add the new branch to the table
                out_from += (To->Length)

                // save the possible updated table in the branches
                branches(From) = out_from
            }
        }
        // check if every station has an outgoing branch
        for (sn <- 0 to branches.length - 1 ) {
            if ( branches(sn) == null )
                static_error.report("Station " + sn + " has no outgoing branches")
        }

        // check if every station has an ingoing branch

        val ingoing = new Array[Boolean](StatNum)
        for(iin <- 0 to ingoing.length - 1) ingoing(iin) = false

       branches.foreach((m) => {
              m.keys.foreach((si)=> {
                  // ingoing branch for station si is found
                  ingoing(si) = true
              })
         })

      for (sn <- 0 to ingoing.length - 1 ) {
        if (!ingoing(sn))
          static_error.report("Station " + sn + " has no ingoing branches")
      }

      for(child<-xml.child) {
        // load from <rote> nodes
        if (child.label == "route") {
          // a <route> node like 	<route Engine ="0">
          val Engine = child.attribute("Engine").get(0).text.toInt
          if ( Engine < 0 || Engine >= EngineNum )
              // invalid Engine attribute
              static_error.report("Incorrect XML node: " + child)
          val engine_route = new ArrayBuffer[Int]() // route of the Engine
          if ( routes(Engine) != null )
              static_error.report("Duplicate route definition for Engine " + Engine)
          routes(Engine) = engine_route
          for(tr_node<-child.child) {
              // load from <track> nodes
              if (tr_node.label == "track") {
                // a <track> node like <track Stat="2"/>
                val Stat = tr_node.attribute("Stat").get(0).text.toInt
                if ( !engine_route.isEmpty && !branches(engine_route.last).contains(Stat) )
                    // the proper branch does not exist
                    static_error.report("Branch " + engine_route.last + "->" + Stat + " doesn't exist")
                // add the Stat to the route
                engine_route += Stat
              }
          }
          routes(Engine) = engine_route
        }
      }
      // check if a route for every engine is defined
      for(ei<- 0 to routes.length - 1)
        if ( routes(ei) == null )
          static_error.report("Route for engine " + ei + " is not defined")
    }

    // find all crashes of this network
    def findCrash : ArrayBuffer[Crash] = {

        // the class keeps arrival the engine to the station at time
        class Arrival (val time:Int, val station:Int, val engine: Int)

        // array of all arrivals
        var arrivals = new ArrayBuffer[Arrival]()

        // gather arrivals for all engines
        for(ei<-0 to routes.length - 1) {
            val en_route = routes(ei) // route of ei engine

            var arr_time = 0;   // time of arrival to the current station
            var prev_si = -1    // previous station with a fake initializer
            for (si<-en_route) {
                if (prev_si >= 0)
                    // the branch from prev_si to the current (si) one
                    arr_time += branches(prev_si)(si)
                arrivals += new Arrival(arr_time, si, ei)
                prev_si = si
            }
        }
        // sorting by (time, station, engine)
        // as a result, collided arrivals become neighbors in the sorted array
        val sorted_arrivals = arrivals.sortWith((x:Arrival, y:Arrival)=>
                    x.time < y.time
                      || x.time == y.time
                         && (x.station < y.station
                              || x.station == y.station && x.engine < y.engine))

        arrivals = sorted_arrivals // throw away the original (unsorted) array

        // extracting crashes the sorted array

        val crashes = new ArrayBuffer[Crash]()

        var crash_time = -1 // time of the crash with a fake initializer
        var crash_stat = -1 // station of the crash with a fake initializer
        var crash_engines = new ArrayBuffer[Int]() // participants of the crash

        for (ar<-arrivals) {
            if ( ar.time == crash_time && ar.station == crash_stat )
              // engines arrive at to ctash_stat at the same crash_time
              crash_engines += ar.engine // the new participant
            else {
                if ( crash_engines.length > 1 ) {
                    // a new crash is found
                    crashes += new Crash(crash_stat, crash_time, crash_engines)
                    crash_engines = new ArrayBuffer[Int]()
                }
                else
                  crash_engines.clear() // avoiding creation a new array object for
                                        // the performance reason

                crash_engines += ar.engine // the first participant of the
                                           // (possible) next crash
                crash_time = ar.time       // time of the next crash
                crash_stat = ar.station    // station of the next crash
            }
        }

        if ( crash_engines.length > 1 )
          // the last crash is detected
          crashes += new Crash(crash_stat, crash_time, crash_engines)

        crashes
    }
}

object CrashDetector {
  // load railway network from a XML file
  def loadNet(path: String) : RailNet = {

      // root xml node like <rail_net StatNum="3" EngineNum="2">
      val xml = scala.xml.XML.loadFile(path)

      if ( xml.label != "rail_net" )
          static_error.report("The root node of configuration must be <rail_net ...>")

      // Number of stations
      val StatNum = xml.attribute("StatNum").get(0).text.toInt
      // Number of engines
      val EngineNum = xml.attribute("EngineNum").get(0).text.toInt

      // network object
      val rn = new RailNet(StatNum, EngineNum)
      rn.loadFromXML(xml) // load from xml object
      rn
  }
  def main(args:Array[String]) : Unit = {
    try {
      println("loading network definition " + args(0) + " ...")
      val rn = loadNet(args(0))
      println("OK")
      println("Analyzing for crashes ...")
      val crashes = rn.findCrash
      println(crashes.length + " crashes detected")

      // printing detected crashes
      var ci = 1
      for (cr <- crashes) {
        println("Crash " + ci); ci += 1
        print("time: " + cr.time + " station: " + cr.station + " engines: ")
        for (ei <- 0 to cr.engines.length - 2)
          print(cr.engines(ei) + ", ")
        println(cr.engines.last) // the last elem with no comma
        println("") // skip one line
      }
    }
    catch {
      case configException(text) => println(text)
      case nse:NoSuchElementException=> println("Error: A XML node doesn't have a required attribute")
    }
  }
}
