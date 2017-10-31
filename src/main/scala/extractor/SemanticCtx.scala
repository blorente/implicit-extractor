package extractor

import java.nio.file.{Files, Path}
import java.util.concurrent.CopyOnWriteArrayList

import org.langmeta.internal.io.PathIO

import scala.collection.mutable
import scala.meta._
import scala.util.control.NonFatal
import org.langmeta.internal.semanticdb.{schema => s}

case class SemanticCtx(database: Database) {
  def input = database.documents.head.input
  def tree: Source = input.parse[Source].get
}

object SemanticdbFileWalker {
  val rootPath = "target/justafew"
  val root = AbsolutePath(rootPath)

  def run[T](f: SemanticCtx => T): mutable.Buffer[(Path, T)] = {
    val results = new CopyOnWriteArrayList[(Path, T)]
    def visit(path: Path): Unit =
      try {
        val sdb = s.Database.parseFrom(Files.readAllBytes(path))
        val mdb = sdb.toDb(None)
        val ctx = SemanticCtx(mdb)
        results.add(path -> f(ctx))
        print(".")
      } catch {
        case NonFatal(e) =>
          val st = e.getStackTrace
          e.setStackTrace(st.take(10))
          e.printStackTrace()
      }
    import scala.collection.JavaConverters._
    val files = Files
      .walk(root.toNIO)
      .iterator()
      .asScala
      .filter { file =>
        Files.isRegularFile(file) &&
          PathIO.extension(file) == "semanticdb"
      }
      .toVector
      .par
    files.foreach(visit)
    results.asScala
  }

}
