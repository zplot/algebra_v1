package algebra

import scala.annotation.tailrec

object Utils {

  case class IntMap(map: Map[Int, Int])

  def combinations[T](size: Int, objects: List[T]) : List[List[T]] = {
    if (size == 0)
      List(List())
    else {
      for {
        x  <- objects
        xs <- combinations(size - 1, objects)
      } yield x :: xs
    }
  }




  // Random integer between 0 and p - 1
  def randomP(p: Int): Int = scala.util.Random.nextInt(p)

  /** Power of n to p
    *
    * Uses repeated squaring algorithm:
    * http://www.algorithmist.com/index.php/Repeated_Squaring
    * 2030534587 = power(3, 27)
    */
  def power( n: Int, p: Int): Int = p match {
    case 0 => 1
    case 1 => n
    case p if p % 2 == 1 => n * power(n * n, (p - 1) / 2)
    case p if p % 2 == 0 => power(n * n, p / 2)
  }

  /** Converts an integral to base 2 in the form of a List[Int]
    * The most significant number to the left.
    *
    * Ex.: toBinary(11) = List(1, 0, 1, 1)
    */
  def toBinary(n: Int): List[Int] = {
    @tailrec
    def loop(n: Int, list: List[Int]): List[Int] = n match {
      case 0 => 0 :: list
      case 1 => 1 :: list
      case n => loop(n/2, n % 2 :: list)
    }
    loop(n, List[Int]())
  }


  // All factors of a number (not only primes)
  def factors(num: Int): Seq[Int] = {
    (1 to num).filter { divisor =>
      num % divisor == 0
    }
  }



  // Power of integers TODO Usar Squaring
  def expInt(n:Int, m: Int): Int = List.fill(m)(n).product


  def primeFactors(number: Int): List[(Int, Int)] = {

    // all prime factors with repetition
    def factorize(x: Int): List[Int] = {
      def loop(x: Int, a: Int): List[Int] = if (a * a > x) List(x) else x % a match {
        case 0 => a :: loop(x / a, a)
        case _ => loop(x, a + 1)
      }
      loop(x, 2)
    }

    val fact = factorize(number)
    val uniq = fact.toSet.toList
    val emptyList: List[(Int, Int)] = List[(Int, Int)]()
    val unflattened = for (i <- uniq) yield (i, fact.count(_ == i)) :: emptyList
    unflattened.flatten
  }

  /** Return the divisors of n. Includes 1 */
  def divisors(n: Int): List[Int] =
    for (i <- List.range(1, n) if n % i == 0) yield i

  /** Return the divisors of n. Includes 1 and n */
  def divisors2(n: Int): List[Int] =
    for (i <- List.range(1, n + 1) if n % i == 0) yield i

  /** Return the common divisors of n and m */
  def commonDivisors(n: Int, m: Int): List[Int] = {
    val tmp1: Set[Int] = divisors(n).toSet
    val tmp2: Set[Int] = divisors(m).toSet
    tmp1.intersect(tmp2).toList
  }

  /** Return if 2 integers has common divisors apart from 1*/
  def haveCommonDivisors(m: Int, n: Int): Boolean = {
    val mDivisors = divisors2(m).toSet // Include 1
    val nDivisors = divisors2(n).toSet // Include 1
    val intersection = mDivisors.intersect(nDivisors)
    intersection.size > 1 // A 1 is always present
  }

  /** coprime*/
  def coprime(m: Int, n: Int): Boolean = !haveCommonDivisors(m, n)

  /** Is 'n' a prime number? */
  def isPrime(n: Int): Boolean = divisors(n).length == 1

  def combinations(size: Int, r: Int) : List[List[Int]] = {
    if (size == 0)
      List(List())
    else {
      for {
        x  <- (0 until r).toList
        xs <- combinations(size-1, r)
      } yield x :: xs
    }
  }

}


