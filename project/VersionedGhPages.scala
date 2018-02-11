import sbt._
import Keys._

object VersionedGhPages {
  def updateIndex(repo: File, apiVersion: (Int, Int)) = Def.task {
    val (major, minor) = apiVersion

    IO.write(repo / "index.html", indexTemplate(major, minor))
  }

  def indexTemplate(major: Int, minor: Int) = s"""
      |<!DOCTYPE html>
      |<html lang="en">test
      |  <head>
      |    <meta charset="UTF-8">
      |    <title>Project Documentation</title>
      |    <script language="JavaScript">
      |    <!--
      |    function doRedirect() {
      |     // redirect to the latest docs
      |      window.location.replace("api/$major.$minor");
      |    }
      |
      |    doRedirect();
      |    //-->
      |    </script>
      |  </head>
      |  <body>
      |    <a href="api/$major.$minor">Go to the project documentation</a>
      |  </body>
      |</html>
    """.stripMargin
}
