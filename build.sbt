organization  := "konfig"

name          := "konfig"

version       := "0.1"

scalaVersion  := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  Seq(
    "com.typesafe"        %   "config"        % "1.3.0",
    "com.chuusai"         %%  "shapeless"     % "2.3.0"
  )
}

scalapropsSettings

scalapropsVersion := "0.3.1"
