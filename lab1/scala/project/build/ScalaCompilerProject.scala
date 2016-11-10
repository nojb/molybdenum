import sbt._

class ScalaCompilerProject(info: ProjectInfo) extends DefaultProject(info) {
    override def outputDirectoryName = "out"
    override def outputPath = "out"
    override def mainScalaSourcePath = "src"
}
