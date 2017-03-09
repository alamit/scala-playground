name := "scala-playground"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter-core" % "0.7",
  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
  "com.github.scala-blitz" %% "scala-blitz" % "1.1",
  "org.scalactic" %% "scalactic" % "2.2.6",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false