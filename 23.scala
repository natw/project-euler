import scala.math.{ sqrt }
import scala.Iterator


def divisors(i: Int): Seq[Int] = {
  (1 to sqrt(i).toInt).filter(i % _ == 0).flatMap{ d: Int =>  List(d, i/d) }.filter(_ < i)
}

def abundantNumbers(min: Int, max: Int): Seq[Int] = {
  (min to max).filter{ i: Int => divisors(i).sum > i }
}

def isSumOfAbundantNums(i: Int): Boolean = {
  var aNums = abundantNumbers(1, i)
  var pairs = aNums.combinations(2) ++ aNums.zip(aNums).map{ z: (Int, Int) => Vector(z._1, z._2) }
  pairs.foreach { pair => 
    if(pair.sum == i) { return true }
  }
  return false
}

def anySum(i: Int, aNums: Seq[Int]): Boolean = {
  aNums.foreach { a1 =>
    aNums.foreach { a2 =>
      if(a1 + a2 == i) { return true }
    }
  }
  false
}

def noAbundantSum(max: Int): Seq[Int] = {
  var noSum: List[Int] = List()
  var aNums = abundantNumbers(1, max)
  var pairs = aNums.combinations(2).toList ++ aNums.map{ n: Int => List(n, n) }
  println(pairs)
  var sums = pairs.map(_.sum).distinct
  println(s"sums: $sums")
  var nums = (1 to max).toList
  println(s"nums: $nums")
  nums.diff(sums)
}

println(noAbundantSum(25))
println(noAbundantSum(28123))
