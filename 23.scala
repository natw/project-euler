import scala.math.{ sqrt }
import scala.Iterator


def divisors(i: Int): Seq[Int] = {
  (1 to sqrt(i).toInt).filter(i % _ == 0).flatMap{ d: Int =>  List(d, i/d) }.filter(_ < i)
}

def abundantNumbers(min: Int, max: Int): Seq[Int] = {
  (min to max).filter{ i: Int => divisors(i).sum > i }
}


def noAbundantSum(max: Int): Seq[Int] = {
  (1 to max).filter { i =>
    var aNums = abundantNumbers(1, i)
    var abundantPairs = aNums.combinations(2) ++ aNums.zip(aNums).map{ z: (Int, Int) => Vector(z._1, z._2) }
    abundantPairs.forall( pair => pair.sum != i )
  }
}

println(noAbundantSum(25))
println(noAbundantSum(28123))
