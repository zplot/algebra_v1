package testing_algebra
import algebra._


object testPolynomialsOverFp extends  App {

  println("Empezamos")

  val cuerpo = Fp(7)
  val anilloDePolis = PolynomialsOverFp(cuerpo)


  implicit def convert1(x: Int): anilloDePolis.R = anilloDePolis.field.build(x)
  implicit def convert2(x: Map[Int, anilloDePolis.R]): anilloDePolis.T = anilloDePolis.Polynomial(x)

  implicit def convert3(x: Map[Int, Int]): anilloDePolis.T = {

    val tmp1 = x.toList
    val tmp2 = tmp1.map(x => (x._1, convert1(x._2)))
    val tmp3 = tmp2.toMap
    convert2(tmp3)

  }


  val poly1Map: Map[Int, anilloDePolis.R] = Map(5 -> 1, 1 -> 4)
  val poly1: anilloDePolis.T = anilloDePolis.Polynomial(poly1Map)

  val poly2Map: Map[Int, anilloDePolis.R] = Map(2 -> 1, 1 -> 1, 0 -> 1)
  val poly2: anilloDePolis.T = anilloDePolis.Polynomial(poly2Map)

  val poly3 = convert2(Map(0 -> 2, 1 -> 3, 2 -> 4, 3 -> 2))
  val poly4 = convert2(Map(4 -> 1, 3 -> 1, 2 -> 1, 1 -> 1, 0 -> 1))

  println(poly1)
  println(poly2)

  println(poly1 + poly2Map)
  println(poly1 - poly2)
  println(poly1 * poly3)

  println("poly1 / poly2 = " + poly1 / poly2)
  val a = 5.toString
  val b = 4.toString
  println(a+b)
  println("Empezamos test de irreducibilidad")
  val siONo = poly4.isIrreducible
  println(poly4)
  println(poly4 + " " + siONo)

//  val p1 = convert2(Map(4 -> 3, 3 -> 1))
//  val p2 = convert2(Map( 1 -> 2))
//
//  val p3 = p1 / p2
//
//  println(p1 + " / " + p2 + " = " + p3)



}

