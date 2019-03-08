scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % "0.16.0",
  "com.lihaoyi" %% "utest" % "0.6.6" % Test
)

testFrameworks += new TestFramework("utest.runner.Framework")

mainClass in assembly := Some("tesi.PageRankEIJMain")

scalacOptions in assembly += "-opt:_"

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => 
   MergeStrategy.discard
 case x => MergeStrategy.first
}
