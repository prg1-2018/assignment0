name         := "assignment1"
scalaVersion := "2.12.3"

scalacOptions ++=        // scalac に与えるオプション
  Seq("-feature",
    "-unchecked",
    "-deprecation",
    "-Xlint")

javaOptions in run ++=   // 仮想機械に与えるオプション
  Seq( "-Xmx2G", "-verbose:gc")                          

// プロジェクトで使う非標準 Scala ライブラリ
//libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.1" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"


// sbt の挙動の設定
//fork in (Test, run) := true
//connectInput := true

// ソースコードの在処を非標準の場所に設定
scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
