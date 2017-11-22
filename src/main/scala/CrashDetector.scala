import scala.collection.immutable._
import scala.xml._

object RailNet {
    // the object encapsulates the railway network definition

    def isLoaded = !branchTab.isEmpty   // is the object loaded?

    def branchLength(st1: Int, st2: Int): Int = {
        // length of branch st1 <-> st2 (if any)
        val v = branchTab.get(NormBranch(st1 min st2, st1 max st2))
        if (v.isEmpty)
            -1            // there is no branch st1 <-> st2
        else
            v.get         // the length of branch st1 <-> st2
    }

    def getStatNumber = statNumber  // number of stations

    def loadFromXml(root: xml.Elem):Unit = {
      // load the object from XML element tree
      assert(!isLoaded)

      statNumber = XmlConfig.getIntAttrib(root, "StatNumber", _ > 1)

      // load <branch> nodes
      val branchList:List[(NormBranch, Int)]  = (for (
            be <- root.child
            if be.label == "branch"
          ) yield {
              val from_att = XmlConfig.getIntAttrib(be, "From",
                                f => f >= 0 && f < statNumber)
              val to_att = XmlConfig.getIntAttrib(be, "To",
                                (t) => t >= 0 && t < statNumber && t != from_att)
              val length_att = XmlConfig.getIntAttrib(be, "Length", l => l > 0)

            (NormBranch(from_att min to_att, from_att max to_att), length_att)
          }).toList

          checkErrors(branchList);      // can be ignored for correct understanding

          branchTab = branchList.toMap;

          assert(isLoaded)
      }

    private var statNumber = -1 // number of stations
    private case class NormBranch (st1: Int, st2: Int) {
      assert(st1 < st2)  // normalized branch st1 <-> st2
    }

    // Table of branches: <NormBranch> => <length>
    private var branchTab: Map[NormBranch, Int] = HashMap[NormBranch, Int]()

    private def checkErrors(brl: List[(NormBranch, Int)]): Unit = {
      // detect errors in definitions of branches

      if (brl.isEmpty)
        error.report("The configuration doesn't have any <branch> nodes")

      // search for ambiguously defined branches
      for ((b1, l1) <- brl; (b2, l2) <- brl)
        if (b1 == b2 && l1 != l2)
          error.report("The Length of branch " + b1.st1 + "<->" + b1.st2 +
            " is ambiguously defined: " + l1 + "/" + l2)
    }
}

object Routes {
    // the object encapsulates definitions of engine's routes

    def getEngineNumber = engineNumber  // Number of engines

    def getEngineRoute(eng: Int): List[Int] = {
        // the route (list of stations) of the engine eng
        val er = routeTab.get(eng)
        if (er.isEmpty) Nil else er.get.stl
      }

    def isLoaded = !routeTab.isEmpty    // Is the object loaded?
    def loadFromXml(root: xml.Elem):Unit = {
        // load the object from XML element tree
        assert(RailNet.isLoaded) // RailNet must be loaded first
        assert(!isLoaded)

        engineNumber = XmlConfig.getIntAttrib(root, "EngineNumber", _ > 0)

        // load <route> nodes
        val routeList: List[EngRoute]  = (for (
          re <- root.child
          if re.label == "route"
        ) yield {
            val engine_att = XmlConfig.getIntAttrib(re, "Engine",
                                      e => e >= 0 && e < getEngineNumber)
            // load <track> nones for the given engine
            val statList :List[Int] = (for (
                te <- re.child
                if te.label == "track"
            ) yield {
              val stat_att = XmlConfig.getIntAttrib(te, "Stat",
                                  s => s >= 0 && s < RailNet.getStatNumber)
              stat_att
            }).toList

            EngRoute(engine_att, statList)
        }).toList

        checkErrors(routeList) // can be ignored for correct understanding

        // produce the map of routs
        routeTab = routeList.map(r=>(r.eng, r)).toMap

        assert(isLoaded)
      }

    private case class EngRoute ( // the route (list of stations) of an engine
                       eng: Int,          // engine
                       stl: List[Int]     // list of stations
                              ) {}

    private var engineNumber = -1    // Number of engines

    // Table of routes: <engine> -> <list of stations>
    private var routeTab :Map[Int, EngRoute] = HashMap[Int, EngRoute]()

    private def checkErrors(erl: List[EngRoute]): Unit = {
        // detect errors in definitions of routes

        def detectDuplicate(erl:List[EngRoute]) : Unit = {
            if (!erl.isEmpty) {
                if (erl.tail.exists(_.eng == erl.head.eng))
                    error.report(error.report("Duplicate route definition for Engine " + erl.head.eng))
                detectDuplicate(erl.tail)
            }
        }
        def checkBranchExist(stl:List[Int]) : Unit = {
            for((st1, st2) <- stl zip stl.tail)
                if (RailNet.branchLength(st1, st2) <= 0)
                    error.report("Branch " + st1 + "->" + st2 + " doesn't exist")
        }

        // check if routes are not duplicated
        detectDuplicate(erl)

        // check if a route for every engine is defined
        for (ei<-List.range(0, getEngineNumber))
            if (!erl.exists(_.eng == ei))
                error.report(error.report("Route for engine " + ei + " is not defined"))

        // check if every branch of an engine's route exists
        for (er<-erl) checkBranchExist(er.stl)
    }
  }

object XmlConfig {
    // the object encapsulates XML representation of configuration

    def isLoaded = !rootElem.isEmpty  // is the object loaded
    def getRoot = {
        assert(isLoaded)
        rootElem.get
    }

    def loadFromFile(path: String): Unit = {
        // load XML definition from file

        rootElem = Some(XML.loadFile(path));

        // check the root element
        if ( isLoaded && rootElem.get.label != "rail_net" )
            error.report("The root node of configuration must be <rail_net ...>")
    }

    def getIntAttrib(elm: xml.Node, attrName: String,
                     isValid:Int=>Boolean = _=>true // validator of the expected value
                  ) : Int = {
        // extract XML attribute and convert its value to Int type
        val ov = elm.attribute(attrName)
        if (ov.isEmpty )
            error.report("A xml node doesn't have attribute " + attrName)
        val iv = ov.get.text.toInt
        if( !isValid(iv) )
            error.report("An invalid value of xml attribute " + attrName + "=" + iv)
        iv
    }
    private var rootElem: Option[xml.Elem] = None
}

object CrashDetector {
    // CrashDetector drives other objects and provides the primary functionality
    def main(args:Array[String]) : Unit = {
    try {
        println("loading network definition " + args(0) + " ...")

        XmlConfig.loadFromFile(args(0))
        assert(XmlConfig.isLoaded)

        RailNet.loadFromXml(XmlConfig.getRoot)
        assert(RailNet.isLoaded)

        Routes.loadFromXml(XmlConfig.getRoot)
        assert(Routes.isLoaded)

        println("OK")
        println("Searching for crashes...")

        val crashes = findCrash

        // sorting detected crashes by time
        val sort_crashes = crashes.sortWith(_.time < _.time)

        println(sort_crashes.length + " crashes detected")

        for ((cr, ci) <-sort_crashes.zipWithIndex)
            println("Crash " + (ci + 1) + ": stations: " + cr.st1 + "<->" + cr.st2 + " engines: " +
            cr.en1 + ", " + cr.en2 + " time: " + cr.time)
    }
    catch {
      case configException(text) => println(text)
    }
    }

    private case class Crash (             // a detected crash
        st1: Int, st2:Int,          // happens between these stations
        en1: Int, en2:Int,          // engines take part
        time : Double               // at that time
                              ) {
        def isSame(cr: Crash) =   time == cr.time &&
            // detecting identical crashes
                      (st1 min st2) == (cr.st1 min cr.st2) &&
                      (st1 max st2) == (cr.st1 max cr.st2) &&
                      (en1 min en2) == (cr.en1 min cr.en2) &&
                      (en1 max en2) == (cr.en1 max cr.en2)
    }
    private case class BranchPass(    // passage between stations
        eng: Int,                     // this engine
        st1:Int, st2:Int,             // passes from st1 to st2
        t1:Int,                       // time of start at st1
        t2:Int                        // time of finish at st2
                                 ) {
        def isCrashed(bp: BranchPass) = eng != bp.eng && st1 == bp.st2 &&
            // detect crashing events
                      st2 == bp.st1 && t1 <= bp.t2 && t2 >= bp.t1

        def crashTime(bp: BranchPass) : Double = {
             // time of the crash
            assert(isCrashed(bp))
            (t1 + bp.t2) / 2.0
        }
    }

    private def collectPass: List[BranchPass] = {
        // produce List[BranchPass] for all engines
        def collectEnginePass(eng: Int) : List[BranchPass] = {
            // produce List[BranchPass] for one engine eng

            // stations that the engine passes
            val stats = Routes.getEngineRoute(eng)

            // making List[BranchPass]
            val (st, time, bpl) =
                stats.tail.foldLeft((stats.head, 0, List[BranchPass]()))((t3, st2)=>{
                                      val (st1, t1, bpl) = t3 // unclosing tuple t3

                                      // time of passing branch st1->st2
                                      val t2 = t1 + RailNet.branchLength(st1, st2)
                                      (st2, t2,
                                        BranchPass(eng, st1, st2, t1, t2) :: bpl)
                })
            bpl // resulting list
        }

        // concatenate lists of all engines
        List.range(0, Routes.getEngineNumber).flatMap(collectEnginePass(_))
    }

    private def findCrash:List[Crash] = {
        // search for all crashes

        def removeDuplicates(crl: List[Crash]): List[Crash] = {
            if (crl.isEmpty) crl else crl.head ::
                    removeDuplicates(crl.tail.filter(!crl.head.isSame(_)))
        }
        val bpl = collectPass // all BranchPasses

        // search for crashes
        val crashList = for (
          bp1 <- bpl;
          bp2 <- bpl;
          if bp1.eng != bp2.eng && (bp1 isCrashed bp2)
      ) yield (Crash(bp1.st1, bp1.st2, bp1.eng, bp2.eng, bp1.crashTime(bp2)))

      removeDuplicates(crashList)   // cleaning duplicated objects
  }
}

// class for error objects
case class configException(text: String) extends Exception(text)

object error {
    // this object provides abnormal termination with reporting a message
    def report(msg: String) = {
        throw configException("Error: " + msg)
    }
}

