package extractor

import org.langmeta.inputs.Input
import scala.meta._

object ImplicitParamsToCSV {

  def apply(): Unit = {
    val results = SemanticdbFileWalker.run { ctx =>
      val file: String = ctx.input match {
        case Input.VirtualFile(path, _) => path
        case Input.File(path, _) => path.toString
        case _ => ""
      }
      val syntheticImplicits =
        for {
          syn <- ctx.dindex.synthetics
          name <- syn.names
          symbol = name.symbol
          den <- ctx.denotation(symbol) if den.isImplicit
        } yield {
          syn -> ImplicitParam(ctx, symbol, den)
        }

      val syntheticTypeParams =
        for {
          syn <- ctx.index.synthetics
          name <- syn.names
          symbol = name.symbol
          den <- ctx.denotation(symbol) if (den.isType || den.isClass || den.isObject || den.isTrait) && !den.isImplicit
        } yield {
          TypeParam(ctx, symbol, den, syn.position.end)
        }

      val syntheticApplies = ctx.index.synthetics.filter(_.names.exists(_.toString() == "apply"))

      val allApps = ctx.tree collect {
        case x: Term.Apply => AppTerm(x, x.args.size, x.fun.pos.end)
        case x: Term.Name => AppTerm(x, 0, x.pos.end)
      }

      val paramsFuns =
        for {
          app <- allApps
          param <- syntheticImplicits collect {
            case (syn, den) if syn.position.end == app.term.pos.end => den
          }
          syntheticApply = syntheticApplies find {
            x => x.position.end >= app.term.pos.start && x.position.end <= app.term.pos.end
          }
        } yield {
          syntheticApply match {
            case Some(synth) => FunApplyWithImplicitParam(SyntheticApply(ctx, synth, file, app.params), param)
            case None => FunApplyWithImplicitParam(FunApply(ctx, app, file), param)
          }
        }

      val params = paramsFuns.groupBy(_.param).keys.toSet
      val funs = paramsFuns.groupBy(_.fun).keys

      CSV.writeCSV(params, "params.csv")
      CSV.writeCSV(funs, "funs.csv")
      CSV.writeCSV(paramsFuns, "params-funs.csv")


      val funsWithTypeParamRelations =
        for {
          app <- allApps
          typeParam <- syntheticTypeParams filter(_.pos == app.nameEnd)
          syntheticApply = syntheticApplies find {
            x => x.position.end >= app.term.pos.start && x.position.end <= app.term.pos.end
          }
        } yield {
          syntheticApply match {
            case Some(synth) => FunApplyWithTypeParam(SyntheticApply(ctx, synth, file, app.params), typeParam)
            case None => FunApplyWithTypeParam(FunApply(ctx, app, file), typeParam)
          }
        }

      val typeParams = funsWithTypeParamRelations.groupBy(_.param).keys.toSet
      val funsWithTypeParams = funsWithTypeParamRelations.groupBy(_.fun).keys

      CSV.writeCSV(typeParams, "typeparams-params.csv")
      CSV.writeCSV(funsWithTypeParams, "typeparams-funs.csv")
      CSV.writeCSV(funsWithTypeParamRelations, "typeparams-relations.csv")
    }

  }
}
