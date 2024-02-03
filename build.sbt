ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "zio-effective",
    idePackagePrefix := Some("io.andrelucas")
  )

lazy val zioVersion = "2.0.13"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion,
  "dev.zio" %% "zio-test-sbt" % zioVersion,
  "dev.zio" %% "zio-streams" % zioVersion,
  "dev.zio" %% "zio-test-junit" % zioVersion
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")