import AssemblyKeys._

lazy val buildSettings = Seq(
  name := "scalation",
  version := "0.9.1",
  scalaVersion := "2.10.3"
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.10.0", 
  "org.scala-lang" % "scala-actors" % "2.10.0"
)

unmanagedSourceDirectories in Compile <++= baseDirectory { base =>
  Seq(
    base / "src/examples/scala"
  )
}

val app = (project in file(".")).
  settings(buildSettings: _*).
  settings(assemblySettings: _*).
  settings(
    // any additional settings here
  )