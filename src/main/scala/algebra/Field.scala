package algebra

trait Field extends Ring {

  type S // Seed
  type T // FieldElement


  trait FieldElement extends RingElement {

    def inverse: T
    def divide(other: T): T

  }
}
