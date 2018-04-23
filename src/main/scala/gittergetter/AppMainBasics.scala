package gittergetter

trait AppMainBasics {
  import scala.collection.mutable.ListMap

  type ArgMap = Map[String, List[String]]

  def argsToMap(args: Array[String]): ArgMap = {
    val argmap = ListMap[String, List[String]]()

    args.foldLeft(argmap)({(m, k:String) => {
      val ss:Seq[Char] = k
      ss match {
        case Seq('-', '-', opt @ _*) => m.put(opt.toString, List[String]())
        case Seq('-', opt @ _*) => m.put(opt.toString, List[String]())
        case opt @ _ => m.put(m.head._1, m.head._2 ++ List[String](opt.toString))
      }
      m
    }})
    Map[String, List[String]](argmap.toList.reverse: _*)
  }

  def hasArg(arg: String, argMap: ArgMap): Boolean = {
    argMap.isDefinedAt(arg)
  }

  def getArg(arg: String, argMap: ArgMap): String = {
    argMap.get(arg).flatMap(_.headOption)
      .getOrElse(sys.error(s"required arg ${arg} not found (--${arg} ...)"))
  }
}
