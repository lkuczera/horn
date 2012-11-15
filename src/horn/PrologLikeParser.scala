package horn
import scala.util.parsing.combinator._
import java.io.Reader

class PrologLikeParser extends RegexParsers {
  val atom = """[\w-]+"""r
  val thingy = ":-"
  val terminator = "."
  def assumption = atom <~ terminator ^^ (name => Atom(Symbol(name)))
  def clause = atom ~ thingy ~ (repsep(atom, ",") <~ terminator) ^^ ({ case atom ~ thingy ~ conditions => Clause(Symbol(atom), conditions.map(Symbol(_)))})
  def prolog = rep(assumption | clause)
}

object PrologLikeParser extends PrologLikeParser {
  implicit def string2atom(str: String) = Atom(Symbol(str))
  def parseProlog(input: Reader) = parseAll(prolog, input)
}

case class Symbol(val name: String) 

case class Atom(symbol: Symbol) extends Clause(symbol, Nil) 

case class Clause(val atom: Symbol, val cond: List[Symbol])  {
  override def toString() = "Clause(%s :- %s)[%s]".format(atom, cond.mkString(","))
}

