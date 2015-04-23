import scala.math.{ sqrt }
import scala.Iterator


def divisors(i: Int): Seq[Int] = {
  (1 to sqrt(i).toInt).filter(i % _ == 0)
}

val triangleNumbers: Iterator[Int] = Iterator.iterate(1, 1){ case (i, t) => (i + 1, t + i + 1) }.map(_._2)

val first = triangleNumbers.collectFirst { case (i) if (divisors(i).length * 2) >= 500 => i }

println(first)
