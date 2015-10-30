//import sbt.keys._

name:="scalacpp-plugin"

organization := "org.scalacpp"

version := "0.0.1"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang"                  %     "scala-reflect"         % scalaVersion.value,
  "org.scala-lang"                  %     "scala-compiler"        % scalaVersion.value      % "provided",
  "org.scala-lang.modules"          %     "scala-xml_2.11"        % "1.0.2",
  "org.scala-lang"                  %     "scala-compiler"        % scalaVersion.value      % "test",
  "commons-io"                      %     "commons-io"            % "2.4"         % "test",
  "org.scalatest"                   %%    "scalatest"             % "2.2.4"       % "test",
  "com.typesafe.scala-logging"      %%    "scala-logging-slf4j"   % "2.1.2"       % "test",
  "org.mockito"                     %     "mockito-all"           % "1.9.5"       % "test",
  "joda-time"                       %     "joda-time"             % "2.3"         % "test",
  "org.joda"                        %     "joda-convert"          % "1.3.1"       % "test",
  "org.slf4j"                       %     "slf4j-api"             % "1.7.7"       % "test",
  "org.scala-lang.modules"          %%    "scala-async"           % "0.9.2"       % "test",
  "com.typesafe.akka"               %%    "akka-actor"            % "2.3.4"       % "test",
  "org.scaldi"                      %%    "scaldi"                % "0.4"         % "test"
)

scalacOptions ++= Seq("-feature", "-deprecation")