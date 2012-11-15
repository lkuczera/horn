package horn

object ParserInferencerTests extends PrologLikeParser {
  val ok =
    """przodek_a_c :- przodek_a_b, przodek_b_c 
    .przodek_b_c :- ojciec_b_c.
przodek_a_b :- matka_a_b.
ojciec_b_c.
matka_a_b."""

  val missingDot = """przodek_a_c :- przodek_a_b, przodek_b_c
przodek_b_c :- ojciec_b_c.
przodek_a_b :- matka_a_b.
ojciec_b_c.
matka_a_b."""

  val loop = """a:-b.b:-c.c:-a."""

  val abc = "a:-b.b:-c.c."

  def parseOk = parseAll(prolog, ok)

  def parseMissingDot = parseAll(prolog, missingDot)

  def parseLoop = parseAll(prolog, loop)

  def parseAbc = parseAll(prolog, abc).get // this will succeed don't try it at home

  def main(args: Array[String]) = {
    println(parseOk)
    println("Parsed ok")
    println(parseMissingDot)
    println("Parsed invalid")
    println(parseLoop)
    println("Parsed loop")
    
    println(Inferencer.infer(parseAbc, Symbol("a")), true)
    println(Inferencer.infer(parseAbc, Symbol("b")), true)
    println(Inferencer.infer(parseAbc, Symbol("c")), true)
    println(Inferencer.infer(parseAbc, Symbol("d")), false)
    println(Inferencer.infer(parseLoop.get, Symbol("d")), false)
    println(Inferencer.infer(parseLoop.get, Symbol("b")), false)
    println(Inferencer.infer(parseOk.get, Symbol("przodek_a_c")), true)
    println(Inferencer.infer(parseOk.get, Symbol("missing")), false)
  }

}