package extractor

object PrintWalk {
  def apply(): String = {
    val results = SemanticdbFileWalker.run { ctx =>
      for {
        name <- ctx.tree.stats
      } yield name.toString
    }

    val sb = new StringBuilder

    def println(s: String) = {
      sb ++= s
      sb ++= "\n"
    }

    results
      .foreach {
        case name =>
          println(s"# $name")
      }
    sb.toString()
  }
}
