lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.12.7"
    )),
    name := "EmbeddedMUPL"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
