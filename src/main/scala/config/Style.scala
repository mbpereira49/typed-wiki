package config

import java.io.File

val cssPath = "src/main/resource/css"

def getCSSFilenames: List[String] = 
  List("markdown.css")

def getCSSFiles: List[File] = 
  val dir = File(cssPath)
  getCSSFilenames.map(File(dir, _))