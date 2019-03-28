import java.io.FileOutputStream
import java.util.Properties

import org.scalajs.core.tools.linker.ModuleKind
import sbt.Keys.{libraryDependencies, resolvers}
import sbtcrossproject.CrossPlugin.autoImport.crossProject
val ivyLocal = Resolver.file("ivy", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)

name := "amf-core"

version in ThisBuild := "4.0.0-SNAPSHOT"

publish := {}

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()

//libraryDependencies += "org.codehaus.sonar.runner" % "sonar-runner-dist" % "2.4"

enablePlugins(SonarRunnerPlugin)

val setSonarProperties = TaskKey[Unit](
  "setSonarProperties",
  "Set sonar properties!"
)

setSonarProperties := {
  lazy val url = sys.env.getOrElse("SONAR_SERVER_URL", "Not found url.")
  lazy val token = sys.env.getOrElse("SONAR_SERVER_TOKEN", "Not found token.")

  val values = Map(
    "sonar.host.url" -> url,
    "sonar.login" -> token,
    "sonar.projectKey" -> "mulesoft.amf",
    "sonar.projectName" -> "AMF",
    "sonar.projectVersion" -> "1.0.0",

    "sonar.sourceEncoding" -> "UTF-8",
    "sonar.github.repository" -> "mulesoft/amf",

    "sonar.modules" -> "amf-core,amf-webapi,amf-aml,amf-validation,amf-client",

    "amf-core.sonar.sources" -> "shared/src/main/scala",
    "amf-core.sonar.scoverage.reportPath" -> "jvm/target/scala-2.12/scoverage-report/scoverage.xml",

    "amf-webapi.sonar.sources" -> "shared/src/main/scala",
    "amf-webapi.sonar.scoverage.reportPath" -> "jvm/target/scala-2.12/scoverage-report/scoverage.xml",

    "amf-aml.sonar.sources" -> "shared/src/main/scala",
    "amf-aml.sonar.scoverage.reportPath" -> "jvm/target/scala-2.12/scoverage-report/scoverage.xml",

    "amf-validation.sonar.sources" -> "shared/src/main/scala",
    "amf-validation.sonar.scoverage.reportPath" -> "jvm/target/scala-2.12/scoverage-report/scoverage.xml",

    "amf-client.sonar.sources" -> "shared/src/main/scala",
    "amf-client.sonar.tests" -> "shared/src/test/scala",
    "amf-client.sonar.scoverage.reportPath" -> "jvm/target/scala-2.12/scoverage-report/scoverage.xml"
  )

  sonarProperties := values

  val p = new Properties()
  values.foreach(v => p.put(v._1, v._2))
  val stream = new FileOutputStream(file("./sonar-project.properties"))
  p.store(stream, null)
  stream.close()
}

val sonarMe = TaskKey[Unit](
  "sonarMe",
  "Run sonar!")
sonarMe := {

  //  sonarRunnerOptions := Seq(
  //    "-D",
  //    s"sonar.host.url=$url",
  //    "-D",
  //    s"sonar.login=$token"
  //  )

  //  val a = generateSonarConfiguration.value

  setSonarProperties.value
  sonar.value
}

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
