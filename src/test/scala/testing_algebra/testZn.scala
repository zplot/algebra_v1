package testing_algebra
import algebra.Zn

object testZn extends App {
  println("Empezamos")

  val ring = Zn(1980)


  println(ring.zeroDivisors)
  println(ring.units)

}