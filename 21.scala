// problem 21 - Amicable numbers

import scala.math.{ sqrt }


def divisors(i: Int): Seq[Int] = {
  (1 to sqrt(i).toInt).filter(i % _ == 0).flatMap{ d: Int =>  List(d, i/d) }.filter(_ < i)
}

def d(n: Int): Int = { divisors(n).sum }

def amicableNumbers(limit: Int): Seq[Int] = {
  (1 to limit).map { i: Int => List(i, d(i)) }
              .filter { case List(i:Int, j:Int) => i < j && d(j) == i }
              .flatten
}

println(amicableNumbers(10000).sum)
