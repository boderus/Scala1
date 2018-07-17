import forcomp.Anagrams.Occurrences

val l1 = List(('a', 3), ('b', 2), ('c', 7))
val l2 = List(('a', 1), ('b', 1))


val m = (l1++l2).groupBy{p => p._1}

val x = m map {case (ch, l) => l match {
  case (_, a)::(_, b)::Nil => (ch, if (a >b) a -b else b - a)
  case p::Nil => p
}}

x.toList.filter(p=> p._2 > 0).sortBy(_._1)


m foreach {
  case (ch, (_, a)::(_, b)::Nil) => (ch, a)
  case (ch, p::Nil) => p
}

//((ch:Char,b:Int) => ch))

/*
def doStuff(occurrences: Occurrences): List[Occurrences]  = {

  /*
  l match {
    case Nil => List(List())
    //case (ch, count) :: tail => (for (b <- 1 to count) yield List((ch, b))).toList
    //case (ch, count) :: tail => (for (b <- 1 to count) {
  }
  */

  //occurrences.toSet.subsets()

}
*/
//for ((ch, count) <- l; b <- 0 to count ) yield List((ch, b))

//l1.toSet.subsets().toList

//doStuff(l)

//def combinations(occurrences: Occurrences): List[Occurrences] = for ((ch, count) <- occurrences; b <- count to 0) yield (ch, b)

/** Subtracts occurrence list `y` from occurrence list `x`.
  *
  *  The precondition is that the occurrence list `y` is a subset of
  *  the occurrence list `x` -- any character appearing in `y` must
  *  appear in `x`, and its frequency in `y` must be smaller or equal
  *  than its frequency in `x`.
  *
  *  Note: the resulting value is an occurrence - meaning it is sorted
  *  and has no zero-entries.
  */

//def subtract(x: Occurrences, y: Occurrences): Occurrences = (x++y).groupBy((ch: Char, i: Int) => ch)