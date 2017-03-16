organization in Global  := "com.quadas"
name in Global          := "konfig"
scalaVersion in Global  := "2.11.8"
crossScalaVersions in Global := Seq("2.11.8", "2.12.0")

lazy val root = project.in(file("."))
  .settings(publish := {})
  .settings(publishLocal := {})
  .aggregate(konfig, `konfig-twitter-util`)

lazy val konfig = project
  .settings(name := "konfig")
  .settings(commonSettings)
  .settings(coreDependencies)
  .settings(wartSettings)
  .settings(publishSettings)

lazy val `konfig-twitter-util` = project
  .settings(name := "konfig-twitter-util")
  .settings(commonSettings)
  .settings(twitterUtilDependencies)
  .settings(wartSettings)
  .settings(publishSettings)
  .dependsOn(konfig)

lazy val commonSettings = Seq(
  scalacOptions := Seq(
    "-deprecation"             // Emit warning and location for usages of deprecated APIs
    , "-encoding", "UTF-8"
    , "-feature"                 // Emit warning and location for usages of features that should be imported explicitly
    , "-unchecked"               // Enable additional warnings where generated code depends on assumptions
    , "-Xfatal-warnings"         // Fail the compilation if there are any warnings
    , "-Xfuture"                 // Turn on future language features
    , "-Xlint"                   // Enable specific warnings (see `scalac -Xlint:help`)
    , "-Yno-adapted-args"        // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver
    , "-Ywarn-dead-code"         // Warn when dead code is identified
    , "-Ywarn-inaccessible"      // Warn about inaccessible types in method signatures
    , "-Ywarn-infer-any"         // Warn when a type argument is inferred to be `Any`
    , "-Ywarn-nullary-override"  // Warn when non-nullary `def f()' overrides nullary `def f'
    , "-Ywarn-nullary-unit"      // Warn when nullary methods return Unit
    , "-Ywarn-numeric-widen"     // Warn when numerics are widened
    , "-Ywarn-unused"            // Warn when local and private vals, vars, defs, and types are unused
    , "-Ywarn-unused-import"     // Warn when imports are unused
    , "-Ywarn-value-discard"     // Warn when non-Unit expression results are unused
  )
)

libraryDependencies in Global ++= Seq(
  "org.scalacheck"      %%  "scalacheck"    % "1.13.4" % "test",
  "org.scalatest"       %%  "scalatest"     % "3.0.1" % "test"
)

lazy val coreDependencies = Seq(
  libraryDependencies ++= Seq(
    "com.typesafe"        %   "config"        % "1.3.1",
    "com.chuusai"         %%  "shapeless"     % "2.3.2"
  )
)

lazy val twitterUtilDependencies = Seq(
  libraryDependencies ++= Seq(
    "com.twitter"   %% "util-core"  % "6.42.0"
  )
)

lazy val wartSettings = Seq(
  wartremoverErrors in (Compile, compile) ++= Seq(
    Wart.Any
    , Wart.Any2StringAdd
    , Wart.AsInstanceOf
    , Wart.DefaultArguments
    , Wart.EitherProjectionPartial
    , Wart.Enumeration
    // , Wart.ExplicitImplicitTypes
    , Wart.IsInstanceOf
    , Wart.JavaConversions
    , Wart.ListOps
    // , Wart.NonUnitStatements
    , Wart.Null
    , Wart.Option2Iterable
    , Wart.OptionPartial
    , Wart.Product
    , Wart.Return
    , Wart.Serializable
    // , Wart.Throw
    , Wart.TryPartial
  )
)

lazy val publishSettings = Seq(
  licenses                += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  bintrayRepository       := "maven",
  bintrayOrganization     := Some("quadas"),
  bintrayReleaseOnPublish := false,

  pomExtra := {
    <url>https://github.com/quadas/konfig</url>
    <scm>
      <connection>scm:git:https://github.com/quadas/konfig.git</connection>
      <developerConnection>scm:git:https://github.com/quadas/konfig.git</developerConnection>
      <url>https://github.com/quadas/konfig</url>
    </scm>
    <developers>
      <developer>
        <name>Yan Su</name>
        <email>yan.su@quadas.com</email>
      </developer>
    </developers>
  }
)
