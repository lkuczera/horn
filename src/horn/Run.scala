package horn
import java.io.File
import java.io.FileReader
import scala.util.parsing.combinator._
import java.io.FileNotFoundException

object Run extends App {
  args.toList match {
    case filename :: question :: Nil => {
      try {
        PrologLikeParser.parseProlog(new FileReader(new File(filename))).map(kb => {
          println(Inferencer.infer(kb, Symbol(question)))
        })
      } catch {
        case e: FileNotFoundException => println("File not found: " + filename)
      }
    }
    case _ => println("Run: [filepath] [question]")
  }

}