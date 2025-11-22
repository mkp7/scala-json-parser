val scala3Version = "3.7.0"

lazy val root = project
  .in(file("."))
  .settings(
    name                                            := "scala-json-parser",
    version                                         := "0.1.0-SNAPSHOT",
    scalaVersion                                    := scala3Version,
    libraryDependencies += "org.scalameta"          %% "munit"                    % "1.0.0" % Test,
    libraryDependencies += "org.scala-lang"         %% "toolkit"                  % "0.7.0",
    libraryDependencies += "org.scalaz"             %% "scalaz-core"              % "7.3.8",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    libraryDependencies += "com.lihaoyi"            %% "pprint"                   % "0.9.0"
  )
