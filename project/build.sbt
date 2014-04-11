libraryDependencies ++= Seq(
  "org.rauschig" % "jarchivelib" % "0.5.0",
  "org.apache.commons" % "commons-compress" % "1.7",
  "commons-io" % "commons-io" % "2.4"
)

unmanagedBase := baseDirectory.value.getParentFile / "lib"

addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.10.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")
