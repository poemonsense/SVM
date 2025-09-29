import mill._, scalalib._

object ivys {
  val scala = "2.13.15"
  val chisel = ivy"org.chipsalliance::chisel:6.7.0"
  val chiselPlugin = ivy"org.chipsalliance:::chisel-plugin:6.7.0"
  val scalameta = ivy"org.scalameta::scalameta:4.9.0"
}

trait SVMModule extends ScalaModule with SbtModule {
  override def scalaVersion = ivys.scala

  override def scalacOptions = Seq("-Ymacro-annotations") ++
    Seq("-feature", "-language:reflectiveCalls")

  override def ivyDeps = Agg(ivys.chisel) ++ Agg(ivys.scalameta)

  override def scalacPluginIvyDeps = Agg(ivys.chiselPlugin)
}

object difftest extends SVMModule

object svm extends SVMModule {
  override def millSourcePath = os.pwd

  override def moduleDeps = Seq(difftest)

  object test extends SbtModuleTests with TestModule.ScalaTest
}
