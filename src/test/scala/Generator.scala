package svm

import chisel3.experimental.ChiselAnnotation
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, CIRCTTarget, CIRCTTargetAnnotation, FirtoolOption}
import difftest.DifftestModule
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, PhaseManager}
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation

class BISTEndpoint(implicit p: SVMParams) extends Endpoint {
  val mod = this.toNamed
  chisel3.experimental.annotate(new ChiselAnnotation {
    def toFirrtl = NestedPrefixModulesAnnotation(mod, "golden_")
  })
}

class GeneratorStage extends ChiselStage {
  // Keep this same as ChiselStage.run except for Dependency[svm.util.VerificationExtractor]
  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    val pm = new PhaseManager(
      targets = Seq(
        Dependency[chisel3.stage.phases.AddImplicitOutputFile],
        Dependency[chisel3.stage.phases.AddImplicitOutputAnnotationFile],
        Dependency[chisel3.stage.phases.MaybeAspectPhase],
        Dependency[chisel3.stage.phases.AddSerializationAnnotations],
        Dependency[chisel3.stage.phases.Convert],
        Dependency[svm.util.VerificationExtractor],
        Dependency[chisel3.stage.phases.AddDedupGroupAnnotations],
        Dependency[chisel3.stage.phases.MaybeInjectingPhase],
        Dependency[circt.stage.phases.AddImplicitOutputFile],
        Dependency[circt.stage.phases.CIRCT],
      ),
      currentState = Seq(
        Dependency[firrtl.stage.phases.AddDefaults],
        Dependency[firrtl.stage.phases.Checks],
      ),
    )
    pm.transform(annotations)
  }
}

object Generator extends App {
  val (p, others) = SVMParams.fromArgs(DifftestModule.parseArgs(args)._1)
  val generator = ChiselGeneratorAnnotation(() => new BISTEndpoint()(p))
  (new GeneratorStage).execute(
    others,
    Seq(generator)
      :+ CIRCTTargetAnnotation(CIRCTTarget.SystemVerilog)
      :+ FirtoolOption("--lowering-options=maximumNumberOfTermsPerExpression=1")
      :+ FirtoolOption("--ignore-read-enable-mem"),
  )
}
