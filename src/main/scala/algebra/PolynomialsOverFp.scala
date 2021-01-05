package algebra

import algebra.Utils._

import scala.annotation.tailrec

case class PolynomialsOverFp(field: Fp ) {

  type Q = Map[Int, Int]
  type R = field.FpElement  // Field elements
  type S = Map[Int, R]      // Maps
  type T = Polynomial       // Polynomials
  val zeroPolynomial: T = Polynomial(Map(0 -> field.zero))
  val onePolynomial: T = Polynomial(Map(0 -> field.one))
  val x: T = Polynomial(Map(1 -> field.one))
  def gcd(g: T, h: T): T = {
    val tmp = gcdExtended(g,h)
    tmp._1
  }
  def gcdExtended(g: T, h: T): (T, T, T) = {

    val r: T = g
    val rPrime: T = h
    val s: T = Polynomial(Map(0 -> field.one))
    val sPrime: T = zeroPolynomial
    val t: T = zeroPolynomial
    val tPrime: T = Polynomial(Map(0 -> field.one))

    @tailrec
    def loop(r: T, s: T, t: T, rPrime: T, sPrime: T, tPrime: T): (T, T, T, T, T, T) = {

      if (rPrime != zeroPolynomial) {
        val (q, rPrimePrime) = r / rPrime
        loop(rPrime, sPrime, tPrime, rPrimePrime, s - sPrime * q, t - tPrime * q)
      } else {
        val c = r.lc
        val (d, tmp2) = r / c
        val sNew = s / c
        val tNew = t / c
        (d, sNew._1, tNew._1, zeroPolynomial, zeroPolynomial, zeroPolynomial)
      }
    }
    val (gcdFinal,sFinal,tFinal, _, dummy2, dummy3) = loop(r, s, t, rPrime, sPrime, tPrime)
    (gcdFinal, sFinal, tFinal)
  }
  def exp(h: T, exponent: Int): T = {
    @tailrec
    def loop(h: Polynomial, exp: Int, acc: Polynomial): Polynomial =
      if (exp <= 1) acc else loop(h, exp - 1, acc * h)
    loop (h, exponent, h)
  }

  class Polynomial private(val map: S)  {

    val degree: Int = {
      //val step1 =  map.keySet
      val step1 = map.keySet
      if (this.map == Map(0 -> field.zero) || this.map == Map[Int, R]()) -999999 else step1.max
    }
    val lc: R = {
      if (degree == -999999) field.zero else this.map(degree)
    }
    def /(other: T): (T, T) = this.divide(other)
    def /(other: R): (T, T) = this.divide(other)
    def divide(other: R): (T, T) = {

      val a: T = this
      val b: T = Polynomial(Map(0 -> other))
      a.divide(b)

    }
    // Ver https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclid.27s_algorithm
    def divide(other: T): (T, T) = {

      val a: T = this
      val b: T = other
      val d: Int = b.degree

      def s(r: T): T = Polynomial(Map( r.degree - d -> r.lc.divide(b.lc)))

      @tailrec
      def loop(q: T, r: T): (T, T) = {
        if (r.degree < d) (q, r) else {
          loop(q + s(r), r - (s(r) * b))
        }
      }
      loop(zeroPolynomial, a)
    }
    def *(other: T): T = this.multiply(other)
    def multiply(other: T): T = { // TODO
      val step1 = for (i <- this.map.toList; j <- other.map.toList) yield (i._1 + j._1, i._2 * j._2)
      val exponents = step1.map(x => x._1).distinct
      val step2 = for (i <- exponents) yield step1.filter(x => x._1 == i)

      def sumListInRing(list1: List[R]): R = list1 match {
        case Nil => field.zero
        case x :: xs => x + sumListInRing(xs)
      }

      def sumCoef(l: List[(Int, R)]): (Int, R) = {
        (l.head._1, sumListInRing(l.map(x => x._2)))
      }

      val step3 = step2.map(sumCoef)
      val step4 = step3.toMap
      Polynomial(step4)

    }
    def +(other: T): T = this.add(other)
    def add(other: T): T = {

      val tmp3 = field.zero
      val exponents = (map.keySet ++ other.map.keySet).toList
      def recursion(exp: List[Int]): S = exp match {
        case Nil => Map[Int, R]()
        case x :: xs => recursion(xs) + (x -> (map.getOrElse(x, tmp3) + other.map.getOrElse(x, tmp3)))
      }
      val temporalMap = recursion(exponents)
      val temporalPoly = Polynomial(temporalMap)
      val result = temporalPoly
      result
    }
    def -(other: T): T = this.minus(other)
    def minus(other: T): T = {
      val tmp1 = other
      val minus1 = field.zero - field.one
      val tmp3 = tmp1 * minus1
      val tmp4 = this.add(tmp3)
      tmp4
    }
    def *(other: R): T = this.multiply(other)
    def multiply(other: R): T = this.multiply(Polynomial(Map(0 -> other)))
    def isMonic: Boolean = this == this.toMonic
    def toMonic: T = {
      val oldMapList = map.toList
      def newMapList(oldMapList: List[(Int, R)]): List[(Int, R)] = oldMapList match {
        case Nil => Nil
        case (x1, x2) :: xs  => (x1, x2.divide(lc)) :: newMapList(xs)
      }
      if (lc == field.one) this else {
        val finalMap = newMapList(oldMapList).toMap
        new Polynomial(finalMap)
      }
    }
    def mod(h: T): T = this.divide(h)._2

    // We will use Rabin's test
    // https://en.wikipedia.org/wiki/Factorization_of_polynomials_over_finite_fields
    def isIrreducible: Boolean = {
      val monic = this.toMonic
      val n = this.degree
      val degreePrimeFactors: List[(Int, Int)] = primeFactors(n)
      val primefactors: List[Int] = degreePrimeFactors.map(x => x._1)
      val k: Int = primefactors.length // Number of prime factors od degree
      val nj: List[Int] = primefactors.map(x => n / x)
      val h1: List[Int] = (0 until k).toList
      def hFunction1(ni: Int) : T = {
        val tmp1: Int = scala.math.pow(field.p, ni).toInt
        val tmp2: T = Polynomial(Map(tmp1 -> field.one))
        val tmp3 = (x / monic)._2
        val h = tmp2 - tmp3
        h
      }
      def xPowerqPowern(n: Int) : T = {
        val tmp1: Int = scala.math.pow(field.p, n).toInt
        val tmp2: T = Polynomial(Map(tmp1 -> field.one))
        tmp2
      }
      val h2 = h1.map(i => gcd(monic, hFunction1(nj(i))))
      val AreallOnes = h2.forall(_ == Polynomial(Map(0 -> field.one)))
      val division = (xPowerqPowern(n) - x) / this
      val g = division._2 // rest
      val is_g_0 = g == zeroPolynomial
      if (!AreallOnes || g != zeroPolynomial) false else true
    }

    // No funciona. Se puede probar con 3x2 + 2x sobre Fp(5)
    def isIrreducible2: Boolean = {

      val n = degree
      val cond1: Boolean = {

        val exponent = math.pow(field.p, degree).toInt
        val xToPn = exp(x, exponent)
        val xToPnMinusX = xToPn - x
        xToPnMinusX.mod(this) == zeroPolynomial
      }

      val cond2: Boolean = {
        val factores = factors(n)
        val factors2 = factores.slice(1, factores.length - 1)
        val tmp1 = for(q <- factors2) yield {
          val exp1 = n/q
          val tmp2 = exp(x, exp1)
          val tmp2MinusX = tmp2 - x
          val tmp4 = gcd(tmp2MinusX, this)
          val tmp5 = tmp4 == onePolynomial
          tmp5
        }
        tmp1.forall(x => x)
      }

      cond1 || cond2
    }



    override def toString: String = {
      def printPol(a: List[(Int, R)]): String = a match {
        case Nil => ""
        case x :: xs if x._1 == 0 => x._2 + " + " + printPol(xs)
        case x :: xs if x._1 == 1 && x._2 == field.one => "x" + " + " + printPol(xs)
        case x :: xs if x._1 == 1 => x._2 + "x" + " + " + printPol(xs)
        case x :: xs if x._2 == field.one => "x" + x._1 + " + " + printPol(xs)
        case x :: xs => x._2 + "x" + x._1 + " + " + printPol(xs)
      }
      def replacePlusMinus(s: String): String = s.replace("+ -", "- ")
      if (this == zeroPolynomial) "0" else replacePlusMinus(printPol(this.map.toList.sortWith(Polynomial.comp)).dropRight(3))
      //map.toString()
    }

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[T]
      if (that == null) false
      else {
        val a1: Boolean = this.map == that.map
        val a2: Boolean = (this.map == Map[Int, R]()) && (that.map == Map(0 -> field.zero))
        val a3: Boolean = (that.map == Map[Int, R]()) && (this.map == Map(0 -> field.zero))
        a1 || a2 || a3
      }
    }

  }

  object Polynomial {

    def apply(map: S): T = {

      val normalMap: S = {
        val theMapList = map.toList
        def newMapList(oldMapList: List[(Int, R)]): List[(Int, R)] = oldMapList match {
          case Nil => Nil
          case (_, field.zero) :: xs => newMapList(xs)
          case (x1, x2) :: xs  => (x1, x2) :: newMapList(xs)
        }
        val theNewMapList = newMapList(theMapList)
        val theNewMap = theNewMapList.toMap
        theNewMap
      }
      new Polynomial(normalMap)
    }

    def mapToPolynomial(intMap: Map[Int, Int]): T = {
      val map1: S = intMap.map(x => (x._1, field.build(x._2)))
      new Polynomial(map1)
    }








    // Creo que esto es para comparar monomios de acuerdo con el grado
    def comp(monomial1: (Int, R), monomial2: (Int, R)): Boolean = monomial1._1 > monomial2._1

  }









}

