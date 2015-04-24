import scala.math.{ sqrt }
import scala.Iterator


def divisors(i: Int): Seq[Int] = {
  (1 to sqrt(i).toInt).filter(i % _ == 0).flatMap{ d: Int =>  List(d, i/d) }.filter(_ < i)
}

def isAbundant(i: Int): Boolean = {
  divisors(i).sum > i
}

def abundantNumbers(min: Int, max: Int): Seq[Int] = {
  (min to max).filter(isAbundant(_))
}

def isSumOfAbundantNums(i: Int): Boolean = {
  (1 to i).foreach { n =>
    if(isAbundant(n) && isAbundant(i-n)) { return true }
  }
  return false
}

def nonAbundantSums(limit: Int): Seq[Int] = {
  (0 to limit).filterNot(isSumOfAbundantNums).toList
}

println(nonAbundantSums(28123).sum)
