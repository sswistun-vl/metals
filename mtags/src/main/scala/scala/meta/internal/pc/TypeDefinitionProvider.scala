package scala.meta.internal.pc

import org.eclipse.{lsp4j => l}
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.pc.OffsetParams
import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerOffsetParams

class TypeDefinitionProvider(val compiler: MetalsGlobal, params: OffsetParams) {
  import compiler._

  def typedTree: Option[(RichCompilationUnit, Position, Tree)] = {
    if (params.isWhitespace) {
      None
    } else {
      val unit = addCompilationUnit(
        code = params.text(),
        filename = params.filename(),
        cursor = None
      )
      val pos = unit.position(params.offset())
      val tree = typedHoverTreeAt(pos)
      Some(unit, pos, tree)
    }
  }

  def typeSymbol: Option[(RichCompilationUnit, Position, Symbol)] = {
    val tri = typedTree
    tri match {
      case Some((t1, t2, tree))
          if tree.symbol.isTypeSymbol || tree.symbol.isMethod || tree.symbol.isConstructor =>
        Some(t1, t2, tree.symbol)
      case Some((t1, t2, tree)) if tree.tpe.isDefined =>
        Some(t1, t2, tree.tpe.typeSymbol)
      case Some((t1, t2, tree)) if tree.children.nonEmpty =>
        Some(t1, t2, tree.children.head.tpe.typeSymbol)
      case Some((t1, t2, tree)) =>
        val expTree = expandRangeToEnclosingApply(tree.pos)
        if (expTree.tpe.isDefined) Some(t1, t2, expTree.tpe.typeSymbol)
        else None
      case _ => None
    }
  }

  def typeDefinition: List[l.Location] = {
    typeSymbol match {
      case Some((_, _, sym))
          if sym.pos.source.file != null && !sym.pos.source.file.isVirtual =>
        val pos = sym.pos

        val src = scala.io.Source.fromFile(sym.pos.source.path)

        val nParams = CompilerOffsetParams(
          pos.source.path,
          src.mkString,
          pos.start
        )
        src.close()

        val provider = new PcDefinitionProvider(compiler, nParams)
        val res = provider.definition()
        res.locations().asScala.toList
      case Some((_, _, sym)) =>
        val pos = sym.pos
        val unit = unitOfFile(pos.source.file)
        unit.body.find(_.symbol == sym) match {
          case Some(value) =>
            value match {
              case t: DefTree =>
                List(new l.Location(pos.source.path, t.namePos.toLSP))
            }
          case None =>
            List(new l.Location(pos.source.path, pos.toLSP))
        }
      case _ => Nil
    }
  }

  def expandRangeToEnclosingApply(pos: Position): Tree = {
    def tryTail(enclosing: List[Tree]): Option[Tree] = enclosing match {
      case Nil => None
      case head :: tail =>
        head match {
          case TreeApply(qual, _) if qual.pos.includes(pos) =>
            tryTail(tail).orElse(Some(head))
          case New(_) =>
            tail match {
              case Nil => None
              case Select(_, _) :: next =>
                tryTail(next)
              case _ =>
                None
            }
          case _ =>
            None
        }
    }
    lastVisistedParentTrees match {
      case head :: tail =>
        tryTail(tail) match {
          case Some(value) =>
            typedTreeAt(value.pos)
          case None =>
            head
        }
      case _ =>
        EmptyTree
    }
  }

  lazy val isForName: Set[compiler.Name] = Set[Name](
    nme.map,
    nme.withFilter,
    nme.flatMap,
    nme.foreach
  )
  def isForSynthetic(gtree: Tree): Boolean = {
    def isForComprehensionSyntheticName(select: Select): Boolean = {
      select.pos == select.qualifier.pos && isForName(select.name)
    }
    gtree match {
      case Apply(fun, List(_: Function)) => isForSynthetic(fun)
      case TypeApply(fun, _) => isForSynthetic(fun)
      case gtree: Select if isForComprehensionSyntheticName(gtree) => true
      case _ => false
    }
  }

  private def typedHoverTreeAt(pos: Position): Tree = {
    val typedTree = typedTreeAt(pos)
    typedTree match {
      case Import(qual, se) if qual.pos.includes(pos) =>
        qual.findSubtree(pos)
      case Apply(fun, args)
          if !fun.pos.includes(pos) &&
            !isForSynthetic(typedTree) =>
        // Looks like a named argument, try the arguments.
        val arg = args.collectFirst {
          case arg if treePos(arg).includes(pos) =>
            arg match {
              case Block(_, expr) if treePos(expr).includes(pos) =>
                // looks like a desugaring of named arguments in different order from definition-site.
                expr
              case a => a
            }
        }
        arg.getOrElse(typedTree)
      case t => t
    }
  }

}
