package algebra

import algebra.Utils.{coprime, divisors}

case class Zn(m: Int) extends Ring {

  type S = Int
  type T = this.ZnElement

  val n: Int = m
  val zero: T = ZnElement(0)
  val one: T = ZnElement(1)
  def build(x: S): T = ZnElement(x)
  override def toString: String = "Z" + m.toString
  val zeroDivisors: List[ZnElement] = divisors(n).filter(_ > 1).map(x => ZnElement(x))
  val units: List[ZnElement] = {
    val tmp1 = List.range(1, n + 1)
    val tmp2 = tmp1.filter(coprime(_, n)).map(x => ZnElement(x))
    tmp2
  }


  case class ZnElement(x: Int) extends RingElement {
    val value: Int = if (x % n < 0) x + n else x
    def add(other: T): T = ZnElement((this.value + other.value) % n)
    def minus(other: T): T = ZnElement((this.value + other.value) % n)
    def multiply(other: T): T = ZnElement((this.value * other.value) % n)
    override def toString: String = value.toString
  }

}


