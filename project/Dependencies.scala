import sbt._

object Dependencies {
  lazy val scalaTest          = "org.scalatest"        %% "scalatest"           % "3.0.5"
  lazy val scalaCheck         = "org.scalacheck"       %% "scalacheck"          % "1.14.0"
  lazy val scalaCheckMagnolia = "com.github.chocpanda" %% "scalacheck-magnolia" % "0.2.2"
}
