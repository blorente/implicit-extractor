package extractor

import scala.meta._

case class EagerInMemorySemanticdbIndex(database: Database) {

  /** Shorthand for scala.meta.Database.documents */
  def documents: Seq[Document] = database.documents

  /** Shorthand for scala.meta.Database.messages */
  def messages: Seq[Message] = database.messages

  /** Shorthand for scala.meta.Database.symbols */
  def symbols: Seq[ResolvedSymbol] = database.symbols

  /** Shorthand for scala.meta.Database.synthetics */
  def synthetics: Seq[Synthetic] = database.synthetics

  object Symbol {
    def unapply(tree: Tree): Option[Symbol] = symbol(tree)
    def unapply(pos: Position): Option[Symbol] = symbol(pos)
  }

  override def hashCode(): Int = database.hashCode()
  private lazy val _denots: Map[Symbol, Denotation] = {
    val builder = Map.newBuilder[Symbol, Denotation]
    database.symbols.foreach(r => builder += (r.symbol -> r.denotation))
    builder.result()
  }
  private lazy val _names: Map[Position, ResolvedName] = {
    val builder = Map.newBuilder[Position, ResolvedName]
    def add(r: ResolvedName) = {
      builder += (r.position -> r)
    }
    database.documents.foreach { entry =>
      entry.names.foreach(add)
      entry.synthetics.foreach(_.names.foreach(add))
      entry.symbols.foreach(_.denotation.names.foreach(add))
    }
    builder.result()
  }
  def symbol(position: Position): Option[Symbol] =
    _names.get(position).map(_.symbol)
  def symbol(tree: Tree): Option[Symbol] = tree match {
    case name @ Name(_) =>
      val syntax = name.syntax
      // workaround for https://github.com/scalameta/scalameta/issues/1083
      val pos =
        if (syntax.startsWith("(") &&
          syntax.endsWith(")") &&
          syntax != name.value)
          Position.Range(name.pos.input, name.pos.start + 1, name.pos.end - 1)
        else name.pos
      symbol(pos)
    case Importee.Rename(name, _) => symbol(name)
    case Importee.Name(name) => symbol(name)
    case Term.Select(_, name @ Name(_)) => symbol(name)
    case Type.Select(_, name @ Name(_)) => symbol(name)
    case _ => symbol(tree.pos)
  }
  def denotation(symbol: Symbol): Option[Denotation] =
    _denots.get(symbol)
  def denotation(tree: Tree): Option[Denotation] =
    symbol(tree).flatMap(denotation)
  def names: Seq[ResolvedName] = _names.values.toSeq

  def withDocuments(documents: Seq[Document]): EagerInMemorySemanticdbIndex =
    copy(database = Database(documents))
}
