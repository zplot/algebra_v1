package testing_algebra
import algebra.Fp



object testFp extends App {
  println("Empezamos")

  val cuerpo = Fp(13)
  println(cuerpo)
  val cuatro = new cuerpo.FpElement(4)
  val nueve = new cuerpo.FpElement(9)
  val c14 = new cuerpo.FpElement(14)

  println(cuerpo.one)
  println(c14)
  println(c14 == cuerpo.one)
  println(nueve.inverse)
  println(nueve * nueve.inverse)



}