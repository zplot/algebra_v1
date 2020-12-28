package testing_algebra
import algebra.Ring
import algebra.Utils

object Z8 extends Ring {
  type S = Int
  type T = this.Hasta7
  val zero: T = new Hasta7(0)
  val one: T = new Hasta7(1)
  def build(x: S): T = new Hasta7(x)

  class Hasta7(x: Int) extends RingElement {
    val value: Int = if (x % 8 < 0) x + 8 else x
    def add(other: T): T = new Hasta7((this.value + other.value) % 8)
    def minus(other: T): T = new Hasta7((this.value + other.value) % 8)
    def multiply(other: T): T = new Hasta7((this.value * other.value) % 8)
    override def toString: String = value.toString
  }

}

object testRing extends App {
  println("Empezamos")

  val cinco = Z8.build(5)
  val cuatro = Z8.build(4)
  println(cuatro + cinco)
  println(cuatro * cinco)
  println("hola")
  val s = Utils.haveCommonDivisors(24, 13)
  println(s)

}




