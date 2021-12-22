package io

import scala.io.Source
import java.io.*
import java.nio.file.{Files, StandardCopyOption, FileSystems}

def read(fileIn: String): String =
  val bufferedSource = Source.fromFile(fileIn)
  val fileContents = bufferedSource.getLines.mkString("\n")
  bufferedSource.close
  fileContents

def copy(source: File, destDir: String): Unit = 
  val dir = File(destDir)
  val dest = File(dir, source.getName)
  dir.mkdirs()
  
  Files.copy(source.toPath(), dest.toPath(), StandardCopyOption.REPLACE_EXISTING);

def write_html(out: String, fileOut: String): Unit = 
  val file = File(fileOut)
  file.getParentFile().mkdirs()
  val bw = BufferedWriter(FileWriter(file))
  try bw.write(out) finally bw.close()