package algebra

import algebra.Utils._

import scala.annotation.tailrec

case class Fp(p: Int) extends Field {
  require(isPrime(p), p + " is not a prime number")

  type S = Int
  type T = this.FpElement

  def build(x: S): T = if (x < 0) FpElement(x % p + p) else FpElement(x % p)


  val zero: T = FpElement(0)
  val one: T = FpElement(1)

  def module(x: Int): Int = if (x < 0) -x else x

  private object FpElement {
    def apply(k: S): FpElement = {
      val v: Int = if (k < 0) {
        (module(k) / p + 1) * p + k
      } else {
        k
      }
      new FpElement(v % p)
    }
  }

  class FpElement(n: Int) extends FieldElement {

    val value: Int = n % p
    def add(other: T): T = FpElement(this.value + other.value)
    override def +(other: T): T = this.add(other)
    def minus(other: T): T = FpElement(this.value - other.value)
    override def -(other: T): T = this.minus(other)
    def multiply(other: T): T = FpElement(this.value * other.value)
    override def *(other: T): T = this.multiply(other)
    def inverse: T = {
      if (this == zero) {
        throw new IllegalArgumentException("zero does not have inverse")
      } else {
        this.power(p - 2)
      }
    }
    def divide(other: T): T = this * other.inverse
    def /(other: T): T = this.divide(other)

    // Power
    def power(exp: Int): T = {
      @tailrec
      def exponen(accum: T, exponente: Int, base: T): T = exponente match {
        case 0 => accum
        case 1 => accum * base
        case q if q % 2 == 1 => exponen(accum * base, (q - 1) / 2, base * base)
        case q if q % 2 == 0 => exponen(accum, q / 2, base * base)
      }
      exponen(one, exp, this)
    }

    override def toString: String = value.toString

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[T]
      if (that == null) false
      else this.value == that.value
    }



  }



}
