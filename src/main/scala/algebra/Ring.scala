package algebra

trait Ring {

  type S // Seed
  type T // RingElement

  val zero: RingElement
  val one: RingElement

  def build(x: S): T

  trait RingElement {

    def add(other: T): T
    def +(other: T): T = this.add(other)
    def minus(other:T): T
    def -(other: T): T = this.minus(other)
    def multiply(other: T): T
    def *(other: T): T = this.multiply(other)

  }

}

