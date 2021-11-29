import scala.io.Source
import java.io._

val fileIn = "test.subst"
val fileOut = "test.html" 

@main def hello = 
  val bufferedSource = Source.fromFile(fileIn)
  val fileContents = bufferedSource.getLines mkString "\n"
  bufferedSource.close
  val parsed = body.parse(fileContents)
  parsed match {
      case Right(x) => 
        val e = x._2
        /*val out = generate(e)
        val file = new File(fileOut)
        val bw = new BufferedWriter(new FileWriter(file))
        try bw.write(out) finally bw.close()*/
        println(e)
      case Left(x) => ()
  }
