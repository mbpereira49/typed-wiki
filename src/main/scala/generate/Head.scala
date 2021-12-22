package generate

import java.io.*

val cssPath = "src/main/resource/css"
val absolute = System.getProperty("user.dir");

def generateHead: String =
  val linkTags = getCSSFilenames.map(filename => 
    s"<link rel=\"stylesheet\" href=\"../../css/$filename\">" 
  )
  linkTags.reduce((x, y) => s"$x\n$y")

def getCSSFilenames: List[String] =
  getCSSFiles.map(_.getName)

def getCSSFiles: List[File] = 
  val d = new File(cssPath)
  if (d.exists && d.isDirectory) then
      d.listFiles.filter(_.isFile).toList
  else List()