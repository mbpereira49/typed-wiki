import scala.io.Source
import java.io._
import cats.parse.Parser

def read_markdown(fileIn: String): String =
  val bufferedSource = Source.fromFile(fileIn)
  val fileContents = bufferedSource.getLines mkString "\n"
  bufferedSource.close
  fileContents

def write(parsed: Either[Parser.Error, Tuple2[String, Body]], fileOut: String): Unit = 
  parsed match {
      case Right(x) => 
        val body = x._2
        //println(x)
        val out = generate(body)
        write_html(out, fileOut)
      case Left(x) => println(x)
  }

def write_html(out: String, fileOut: String): Unit = 
  val file = new File(fileOut)
  val bw = new BufferedWriter(new FileWriter(file))
  try bw.write(out) finally bw.close()