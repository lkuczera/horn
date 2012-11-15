package horn
import scala.util.parsing.combinator._
import java.io.Reader
/**
 * Basic prolog like parser, can be easily extended to parse more prolog syntax.
 * Now it can parse something like:
 * a :- b.
 * b :- c.
 * c.
 */
class PrologLikeParser extends RegexParsers {
  val atom = """[\w-]+"""r // basic word
  val thingy = ":-" //  
  val terminator = "." // end on sentence
  def assumption = atom <~ terminator ^^ (name => Atom(Symbol(name)))
  def clause = atom ~ thingy ~ (repsep(atom, ",") <~ terminator) ^^ ({ case atom ~ thingy ~ conditions => Clause(Symbol(atom), conditions.map(Symbol(_)))})
  def prolog = rep(assumption | clause)
}

object PrologLikeParser extends PrologLikeParser {
  implicit def string2atom(str: String) = Atom(Symbol(str))
  def parseProlog(input: Reader) = parseAll(prolog, input)
}

// basic syntax element
case class Symbol(val name: String) 

// Atom is special case of one element clause
case class Atom(symbol: Symbol) extends Clause(symbol, Nil) 

// Clause has an atom and conditions that has to be inferred in order to reason about atom
case class Clause(val atom: Symbol, val cond: List[Symbol])  {
  override def toString() = "Clause(%s :- %s)[%s]".format(atom, cond.mkString(","))
}

