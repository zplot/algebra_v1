package testing_algebra
import algebra._
import testing_algebra.testPolynomialsOverFp.{anilloDePolis, cuerpo}


object testPolynomialsOverFp extends  App {

  println("Empezamos")

  val cuerpo = Fp(7)
  val anilloDePolis = PolynomialsOverFp(cuerpo)



  println("XXXXXXXXXXXXXXXXXXXXXXXXXX")

  val p7 = anilloDePolis.Polynomial(Map(1 -> anilloDePolis.field.zero, 3 -> anilloDePolis.field.zero))
  println("p7 = " + p7)
  println(p7 == anilloDePolis.zeroPolynomial)
  println("p7 == anilloDePolis.zeroPolynomial = " + (p7 == anilloDePolis.zeroPolynomial))
  println("p7.map = " + p7.map)

  def convert1(a: Map[Int, Int]): anilloDePolis.T ={
    val b = a.map(x => (x._1, anilloDePolis.field.build(x._2)))
    anilloDePolis.Polynomial(b)
  }

  def convert2(cuerpo: Fp, a: Map[Int, Int]) = {
    val anillo = PolynomialsOverFp(cuerpo)
    val b = a.map(x => (x._1, anillo.field.build(x._2)))
    anillo.Polynomial(b)
  }

  def convert3(p: Int, a: Map[Int, Int]) = {
    val cuerpo = Fp(p)
    val anillo = PolynomialsOverFp(cuerpo)
    val b = a.map(x => (x._1, anillo.field.build(x._2)))
    anillo.Polynomial(b)
  }



  val p = convert3(5, Map(1 -> 2, 2 -> 3))
  println(p)


















}

