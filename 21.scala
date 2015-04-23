// problem 21 - Amicable numbers

import scala.math.{ sqrt }


def divisors(i: Int): Seq[Int] = {
  (1 to sqrt(i).toInt).filter(i % _ == 0).flatMap{ d: Int =>  List(d, i/d) }.filter(_ < i)
}

def d(n: Int): Int = { divisors(n).sum }

def areAmicable(i: Int, j: Int): Boolean = { d(i) == j && d(j) == i }

def amicableNumbers(limit: Int): Seq[Int] = {
  var nums: Array[Int] = Array()
  for( i <- 1 to limit ) {
    for( j <- i+1 to limit ) {
      if( !nums.contains(j) ) {
        if( areAmicable(i, j) ) {
          nums = nums :+ i
          nums = nums :+ j
        }
      }
    }
  }
  return nums
}

println(divisors(220))
println(d(220))
println(divisors(284))
println(d(284))
println(areAmicable(220, 284))

var amNums: Seq[Int] = amicableNumbers(10000)
println(amNums.sum)

