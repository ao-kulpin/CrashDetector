import scala.annotation.tailrec
import scala.collection.immutable._
import scala.xml._

object XmlUtils {
  /**
    * extract XML attribute and convert its value to Int type
    */
  @throws[configException]
  def getIntAttrib(elm: xml.Node, attrName: String,
                   isValid: Int => Boolean = _ => true // validator of the expected value
                  ): Int = {
    val ov = elm.attribute(attrName)
    error.require(ov.nonEmpty,s"A xml node doesn't have attribute $attrName")
    val iv = ov.get.text.toInt
    error.require( isValid(iv), s"An invalid value of xml attribute $attrName = $iv")
    iv
  }

}

/**
  * Encapsulates the railway network definition
  */
object RailNet {
  import XmlUtils.getIntAttrib

  def isLoaded: Boolean = branchTab.nonEmpty

  def branchLength(st1: Int, st2: Int): Int = {
    // length of branch st1 <-> st2 (if any)
    val v = branchTab.get(NormBranch(st1 min st2, st1 max st2))
    if (v.isEmpty)
      -1 // there is no branch st1 <-> st2
    else
      v.get // the length of branch st1 <-> st2
  }

  def stationCount: Int = statNumber

  def loadFromXml(root: xml.Elem): Unit = {
    assert(!isLoaded)

    statNumber = getIntAttrib(root, "StatNumber", _ > 1)

    // load <branch> nodes
    val branchList: List[(NormBranch, Int)] = (for (
      be <- root.child
      if be.label == "branch"
    ) yield {
      val from_att = getIntAttrib(be, "From",
        f => f >= 0 && f < statNumber)
      val to_att = getIntAttrib(be, "To",
        (t) => t >= 0 && t < statNumber && t != from_att)
      val length_att = getIntAttrib(be, "Length", l => l > 0)

      (NormBranch(from_att min to_att, from_att max to_att), length_att)
    }).toList

    checkErrors(branchList) // can be ignored for correct understanding

    branchTab = branchList.toMap

    assert(isLoaded)
  }

  private var statNumber = -1 // number of stations
  private case class NormBranch(st1: Int, st2: Int) {
    assert(st1 < st2) // normalized branch st1 <-> st2
  }

  /**
    * Table of branches: [[NormBranch]] -> length
    */
  private var branchTab: Map[NormBranch, Int] = HashMap[NormBranch, Int]()

  private def checkErrors(brl: List[(NormBranch, Int)]): Unit = {
    // detect errors in definitions of branches

    error.require(brl.nonEmpty, "The configuration doesn't have any <branch> nodes")

    // search for ambiguously defined branches
    for ((b1, l1) <- brl; (b2, l2) <- brl)
      error.require (
        !(b1 == b2 && l1 != l2),
        s"The Length of branch ${b1.st1} <-> ${b1.st2}  is ambiguously defined: $l1 / $l2"
      )
  }
}

/**
  * the object encapsulates definitions of engine's routes
  */
object Routes {
  import XmlUtils.getIntAttrib

  def engineCount: Int = engineNumber

  def getEngineRoute(eng: Int): List[Int] = {
    // the route (list of stations) of the engine eng
    val er = routeTab.get(eng)
    if (er.isEmpty) Nil else er.get.stl
  }

  def isLoaded: Boolean = routeTab.nonEmpty

  /**
    *
    * load the object from XML element tree
    *
    * @param root xml node containing the configuration
    */
  def loadFromXml(root: xml.Elem): Unit = {
    assert(RailNet.isLoaded) // RailNet must be loaded first
    assert(!isLoaded)

    engineNumber = getIntAttrib(root, "EngineNumber", _ > 0)

    /**
      * load route nodes
      */
    val routeList: List[EngRoute] = (for (
      re <- root.child
      if re.label == "route"
    ) yield {
      val engine_att = getIntAttrib(re, "Engine",
        e => e >= 0 && e < engineCount)
      // load <track> nones for the given engine
      val statList: List[Int] = (for (
        te <- re.child
        if te.label == "track"
      ) yield {
        val stat_att = getIntAttrib(te, "Stat",
          s => s >= 0 && s < RailNet.stationCount)
        stat_att
      }).toList

      EngRoute(engine_att, statList)
    }).toList

    checkErrors(routeList) // can be ignored for correct understanding

    // produce the map of routs
    routeTab = routeList.map(r => (r.eng, r)).toMap

    assert(isLoaded)
  }

  private case class EngRoute( // the route (list of stations) of an engine
                               eng: Int, // engine
                               stl: List[Int] // list of stations
                             )

  private var engineNumber = -1 // Number of engines

  /**
    * Table of routes: engine -> list of stations
    */
  private var routeTab: Map[Int, EngRoute] = HashMap[Int, EngRoute]()

  private def checkErrors(erl: List[EngRoute]): Unit = {
    // detect errors in definitions of routes

    @tailrec
    def detectDuplicate(erl: List[EngRoute]): Unit =
      if (erl.nonEmpty) {
        error.require(!erl.tail.exists(_.eng == erl.head.eng),
          s"Duplicate route definition for Engine ${erl.head.eng}")
        detectDuplicate(erl.tail)
      }


    def checkBranchExist(stl: List[Int]): Unit = {
      for ((st1, st2) <- stl zip stl.tail)
        error.require(RailNet.branchLength(st1, st2) > 0, s"Branch $st1 -> $st2 doesn't exist")
    }

    detectDuplicate(erl)

    // check if a route for every engine is defined
    for (ei <- List.range(0, engineCount))
      error.require(erl.exists(_.eng == ei), s"Route for engine $ei + is not defined")

    // check if every branch of an engine's route exists
    for (er <- erl) checkBranchExist(er.stl)
  }
}


/**
  * Drives other objects and provides the primary functionality
  */
object CrashDetector {
  def main(args: Array[String]): Unit = {
    try {
      println(s"loading network definition ${args(0)}  ...")

      val xml = XML.loadFile(args(0))
      error.require(xml.label == "rail_net", "The root node of configuration must be <rail_net ...>")

      RailNet.loadFromXml(xml)
      assert(RailNet.isLoaded)

      Routes.loadFromXml(xml)
      assert(Routes.isLoaded)

      println("OK")
      println("Searching for crashes...")

      val crashes = findCrashes

      // sorting detected crashes by time
      val sort_crashes = crashes.sortWith(_.time < _.time)

      println(sort_crashes.length + " crashes detected")

      for ((cr, ci) <- sort_crashes.zipWithIndex)
        println("Crash " + (ci + 1) + ": stations: " + cr.st1 + "<->" + cr.st2 + " engines: " +
          cr.en1 + ", " + cr.en2 + " time: " + cr.time)
    }
    catch {
      case configException(text) => println(text)
    }
  }

  private case class Crash( // a detected crash
                            st1: Int, st2: Int, // happens between these stations
                            en1: Int, en2: Int, // engines take part
                            time: Double // at that time
                          ) {
    def isSame(cr: Crash): Boolean = time == cr.time &&
      // detecting identical crashes
      (st1 min st2) == (cr.st1 min cr.st2) &&
      (st1 max st2) == (cr.st1 max cr.st2) &&
      (en1 min en2) == (cr.en1 min cr.en2) &&
      (en1 max en2) == (cr.en1 max cr.en2)
  }

  /**
    * Passage between stations
    * @param eng this engine
    * @param st1 station we started from
    * @param st2 stations we went to
    * @param t1  time of start at t1
    * @param t2  time of finish at st2
    */
  private case class BranchPass(
                                 eng: Int,
                                 st1: Int, st2: Int,
                                 t1: Int,
                                 t2: Int
                               ) {
    def isCrashed(bp: BranchPass): Boolean = eng != bp.eng && st1 == bp.st2 &&
      // detect crashing events
      st2 == bp.st1 && t1 <= bp.t2 && t2 >= bp.t1

    def crashTime(bp: BranchPass): Double = {
      // time of the crash
      assert(isCrashed(bp))
      (t1 + bp.t2) / 2.0
    }
  }

  private def collectPass: List[BranchPass] = {
    // produce List[BranchPass] for all engines
    def collectEnginePass(eng: Int): List[BranchPass] = {
      // produce List[BranchPass] for one engine eng

      // stations that the engine passes
      val stats = Routes.getEngineRoute(eng)

      // making List[BranchPass]
      val (st, time, bpl) =
        stats.tail.foldLeft((stats.head, 0, List[BranchPass]()))((t3, st2) => {
          val (st1, t1, bpl) = t3 // unclosing tuple t3

          // time of passing branch st1->st2
          val t2 = t1 + RailNet.branchLength(st1, st2)
          (st2, t2,
            BranchPass(eng, st1, st2, t1, t2) :: bpl)
        })
      bpl // resulting list
    }

    // concatenate lists of all engines
    List.range(0, Routes.engineCount).flatMap(collectEnginePass)
  }

  private def findCrashes: List[Crash] = {
    def removeDuplicates(crl: List[Crash]): List[Crash] = {
      if (crl.isEmpty) crl else crl.head ::
        removeDuplicates(crl.tail.filter(!crl.head.isSame(_)))
    }

    val bpl = collectPass // all BranchPasses

    // search for crashes
    val crashList = for (
      bp1 <- bpl;
      bp2 <- bpl
      if bp1.eng != bp2.eng && (bp1 isCrashed bp2)
    ) yield Crash(bp1.st1, bp1.st2, bp1.eng, bp2.eng, bp1.crashTime(bp2))

    removeDuplicates(crashList) // cleaning duplicated objects
  }
}

case class configException(text: String) extends Exception(text)

object error {
  private def report(msg: String): Nothing = throw configException(s"Error: $msg")
  def require(should : Boolean, msg : ⇒ String) : Unit =
    if (!should) report(msg)
}

