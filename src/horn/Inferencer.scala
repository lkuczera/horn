package horn

object Inferencer {

  def infer(kb: List[Clause], q: Symbol): Boolean = {
    //a table, indexed by clause, initially the number of premises
    //this keeps track of how many premises of each implication are as yet unknown. 
    // whenever a new symbol p from the agenda is processed, the count is reduced by one for
    /// each implication in whose premise p appears.
    // if a count reaches zero, all the premises of the implication are known so its conclusion can be added to the agenda.
    var count = Map[Clause, Int]();
    kb.foreach(clause => { count += (clause -> clause.cond.size) })

    // table, indexed by symbol, each entry initially false
    // this keeps track of which symbols have been processed. an inferred symbol need not be added to the agenda
    // if it has been processed previously. This avoids redundant work. it also prevents infinite loops that could be 
    // caused by implications such as P => Q and Q => P
    var inferred = Map[Symbol, Boolean]();
    kb.foreach(_.cond.foreach(cond => inferred += (cond -> false)))

    // a list of symbols, initially the symbols known to be true in KB
    // this keeps track of symbols known to be true but not yet "processed"
    var agenda = kb.filter(_.cond.isEmpty).map(_.atom)

    //go till the agenda has something 
    while (!agenda.isEmpty) {
      val p = agenda.head; agenda = agenda.tail; // p = POP(agenda)
      //do the stuff only if p has not been inferred already
      if (!inferred.get(p).getOrElse(false)) {
        inferred += (p -> true)
        count.filterKeys(_.cond.contains(p)).foreach(clauseNum => {
          val clause = clauseNum._1
          val cnt = clauseNum._2
          count += (clause -> (cnt - 1))
          if (count(clause) == 0) {
            if (clause.atom == q) return true
            agenda = agenda ::: List(clause.atom)
          }
        })
      }
    }
    false
  }
}