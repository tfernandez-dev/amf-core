import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers}
import sbtcrossproject.CrossPlugin.autoImport.crossProject
val ivyLocal = Resolver.file("ivy", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)

name := "amf-core"

version in ThisBuild := "4.0.0-SNAPSHOT"

publish := {}

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()

//libraryDependencies += "org.codehaus.sonar.runner" % "sonar-runner-dist" % "2.4"

val settings = Common.settings ++ Common.publish ++ Seq(
  organization := "com.github.amlorg",
  resolvers ++= List(ivyLocal, Common.releases, Common.snapshots, Resolver.mavenLocal),
  resolvers += "jitpack" at "https://jitpack.io",
  credentials ++= Common.credentials(),
  libraryDependencies ++= Seq(
    "org.scalatest"    %%% "scalatest" % "3.0.5" % Test,
    "com.github.scopt" %%% "scopt"     % "3.7.0"
  )
)

/** **********************************************
  * AMF-Core
  * ********************************************* */
lazy val defaultProfilesGenerationTask = TaskKey[Unit](
  "defaultValidationProfilesGeneration",
  "Generates the validation dialect documents for the standard profiles")

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .settings(
    Seq(
      name := "amf-core",
      libraryDependencies += "org.mule.syaml" %%% "syaml" % "0.6.8"
    ))
  .in(file("."))
  .settings(settings)
  .jvmSettings(
    libraryDependencies += "org.scala-js"           %% "scalajs-stubs"          % scalaJSVersion % "provided",
    libraryDependencies += "org.scala-lang.modules" % "scala-java8-compat_2.12" % "0.8.0",
    libraryDependencies += "org.json4s"             %% "json4s-native"          % "3.5.4",
    artifactPath in (Compile, packageDoc) := baseDirectory.value / "target" / "artifact" / "amf-core-javadoc.jar"
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2",
    scalaJSModuleKind := ModuleKind.CommonJSModule,
    artifactPath in (Compile, fullOptJS) := baseDirectory.value / "target" / "artifact" / "amf-core-module.js"
  )

lazy val coreJVM = core.jvm.in(file("./jvm"))
lazy val coreJS  = core.js.in(file("./js"))
