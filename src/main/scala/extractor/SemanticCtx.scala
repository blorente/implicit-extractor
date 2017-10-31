package extractor

import java.nio.file.{Files, Path}
import java.util.concurrent.CopyOnWriteArrayList

import org.langmeta.internal.io.PathIO

import scala.collection.mutable
import scala.meta._
import scala.util.control.NonFatal
import org.langmeta.internal.semanticdb.{schema => s}


case class SemanticCtx(database: Database, projectPath: AbsolutePath) {
  def input = database.documents.head.input
  val file: String = input match {
    case Input.VirtualFile(path, _) => path
    case Input.File(path, _) => path.toString
    case _ => ""
  }

  def tree: Source = input.parse[Source].get
  implicit val index = EagerInMemorySemanticdbIndex(database)
  def dindex = index.withDocuments(index.documents.filter(_.input == input))

  def denotation(s: Symbol): Option[Denotation] = dindex.denotation(s)
  def denotation(t: Tree): Option[Denotation] = dindex.denotation(t)


  def qualifiedName(symbol: Term): String = {
    symbol match {
      case fun: Term.Name => {
        s"${index.symbol(fun).getOrElse(s"<unknown fun: ${fun}>")}"
      }
      case fun: Term.Select => {
        s"${index.symbol(fun.name).getOrElse(qualifiedName(fun.name))}"
      }
      case fun: Term.ApplyType => {
        qualifiedName(fun.fun)
      }
      case fun: Term.Apply => {
        index.symbol(fun).getOrElse(qualifiedName(fun.fun)).toString
      }
      case fun: Term.ApplyInfix => {
        index.symbol(fun).getOrElse(qualifiedName(fun.op)).toString
      }
      case other => {
        Console.withOut(Console.err) { println(s"[error] Function type unknown: ${other.structure}") }
        throw new RuntimeException()
      }
    }
  }

  def getKind(denot: Denotation): String = {
    denot match {
      case x: Denotation if x.isVal && x.isLazy => "lazy val"
      case x: Denotation if x.isVal => "val"
      case x: Denotation if x.isVar => "var"
      case x: Denotation if x.isDef => "def"
      case x: Denotation if x.isObject => "object"
      case x: Denotation if x.isParam => "param"
      case x: Denotation if x.isMacro => "macro"
      case x: Denotation => s"<unknown: ${x.structure}>"
    }
  }

  def getTypeKind(denot: Denotation): String = {
    denot match {
      case den => {
        var kind: String = den match {
          case x if x.isClass && x.isCase => "case class"
          case x if x.isClass && !x.isCase => "class"
          case x if x.isObject => "object"
          case x if x.isTrait => "trait"
          case _ => ""
        }
        if (den.isImplicit) kind = s"implicit $kind"
        if (den.isFinal) kind = s"final $kind"
        if (den.isLazy) kind = s"lazy $kind"
        kind
      }
    }
  }
}

object SemanticdbFileWalker {
  val rootPath = "target/justafew"
  val root = AbsolutePath(rootPath)

  def run[T](f: SemanticCtx => T): Unit = {
    var projectPath: AbsolutePath = root
    val results = new CopyOnWriteArrayList[T]
    def visit(path: Path): Unit =
      try {
        val sdb = s.Database.parseFrom(Files.readAllBytes(path))
        val mdb = sdb.toDb(None)
        val ctx = SemanticCtx(mdb, projectPath)
        results.add(f(ctx))
        print(".")
      } catch {
        case NonFatal(e) =>
          val st = e.getStackTrace
          e.setStackTrace(st.take(10))
          e.printStackTrace()
      }
    import scala.collection.JavaConverters._
    val dirs = Files.list(root.toNIO)
    dirs.forEach { project =>
      projectPath = AbsolutePath(s"$rootPath/${project.getFileName}")
      val files = Files
        .walk(project.toAbsolutePath)
        .iterator()
        .asScala
        .filter { file =>
          Files.isRegularFile(file) &&
            PathIO.extension(file) == "semanticdb"
        }
        .toVector
        .par
      files.foreach(visit)
    }
  }
}
