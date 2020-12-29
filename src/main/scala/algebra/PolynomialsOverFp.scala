package algebra

import scala.annotation.tailrec

case class PolynomialsOverFp(field: Fp ) {

  type R = field.FpElement  // Field elements
  type S = Map[Int, R]      // Maps
  type T = Polynomial       // Polynomials

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

    // Creo que esto es para comparar monomios de acuerdo con el grado
    def comp(monomial1: (Int, R), monomial2: (Int, R)): Boolean = monomial1._1 > monomial2._1

  }



  val zeroPolynomial: Polynomial = Polynomial(Map(0 -> field.zero))

  val x: T = Polynomial(Map(1 -> field.one))



  class Polynomial private(val map: S)  {


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

    def minus(other: T): T = {
      val tmp1 = other
      val minus1 = field.zero - field.one
      val tmp3 = tmp1 * minus1
      val tmp4 = this.add(tmp3)
      tmp4
    }

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

    val degree: Int = {
      //val step1 =  map.keySet
      val step1 = map.keySet
      if (this.map == Map(0 -> field.zero) || this.map == Map[Int, R]()) -999999 else step1.max
    }

    val lc: R = {
      if (degree == -999999) field.zero else this.map(degree)
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

    def divide(other: R): (T, T) = {

      val a: T = this
      val b: T = Polynomial(Map(0 -> other))
      a.divide(b)

    }

    def multiply(other: R): T = this.multiply(Polynomial(Map(0 -> other)))

    def *(other: T): T = this.multiply(other)
    def *(other: R): T = this.multiply(other)

    def +(other: T): T = this.add(other)

    def -(other: T): T = this.minus(other)

    def /(other: T): (T, T) = this.divide(other)
    def /(other: R): (T, T) = this.divide(other)

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

    def isMonic: Boolean = this == this.toMonic

    def mod(h: T): T = this.divide(h)._2

    def isIrreducible: Boolean = ???

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
      if (this == zeroPolynomial) "0" else replacePlusMinus(printPol(this.map.toList.sortWith(Polynomial.comp)).dropRight(2))
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









}

