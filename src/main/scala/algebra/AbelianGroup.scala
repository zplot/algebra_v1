package algebra

trait AbelianGroup {

  type S // Seed
  type T // Element

  val zero: Element

  def build(x: S): Element

  trait Element {

    def add(other:T): T
    def +(other:T): T = this.add(other)
    def minus(other:T): T
    def -(other:T): T = this.minus(other)
    def inverse: T

  }

}

/*
class Zn(m: Int, x: Int) extends AbelianGroup {

  type T = ZnElement

  val value: Int = x % m
  val n: Int = m

  def add(other: T): T= new ZnElement(n, this.value + other.value)
  def +(other: T): T= this.add(other)

}

class Zn(ene: Int) extends AbelianGroup {

  type T = ZnElement

  val n: Int = ene
  val zero = new ZnElement(n, 0)

}*/





