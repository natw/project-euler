import scala.math.{ sqrt }
import scala.Iterator


def divisors(i: Int): Seq[Int] = {
  (1 to sqrt(i).toInt).filter(i % _ == 0).flatMap{ d: Int =>  List(d, i/d) }.filter(_ < i).distinct
}

def divisors2(x : Int): List[Int] =
  1 :: (for(i <- 2 to Math.sqrt(x).toInt if x % i == 0) yield {
    List(i,(x/i).toInt)
  }).toList.flatten.distinct

def isAbundant(i: Int): Boolean = {
  divisors(i).sum > i
}

def abundantNumbers(min: Int, max: Int): Seq[Int] = {
  (min to max).filter(isAbundant(_))
}

def sumOfNonAbundantSums(limit: Int): Int = {
  var aNums = abundantNumbers(1, limit)
  var nums = Array.range(1, limit+1)
  for(
    i <- aNums;
    j <- aNums.dropWhile(_ != i ) if i + j <= limit
  ) {
    nums(i+j-1) = 0
  }
  nums.sum
}


println(sumOfNonAbundantSums(28123))


