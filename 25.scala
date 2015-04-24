import scala.math.{ pow, sqrt, log10 }

val phi: Double = 1.61803398874989484820458683436563811772030917980576

def digitsInFib(n: Int): Int = {
  (n * log10(phi) - log10(sqrt(5)) + 1.0).toInt
}

val q = Iterator.from(1).collectFirst { case n if digitsInFib(n) >= 1000 => n }

println(q.get)
