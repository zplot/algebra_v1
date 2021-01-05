package testing_algebra
import algebra._



object testPolynomialsOverFp extends  App {

  println("Empezamos")

  val cuerpo = Fp(7)
  val anilloDePolis = PolynomialsOverFp(cuerpo)

  println("XXXXXXXXXXXXXXXXXXXXXXXXXX")

  val p7 = anilloDePolis.Polynomial(Map(1 -> anilloDePolis.field.build(5), 3 -> anilloDePolis.field.build(2)))
  val p8 = anilloDePolis.Polynomial(Map(1 -> anilloDePolis.field.build(4), 5 -> anilloDePolis.field.build(3)))
  println("p7 = " + p7)
  println("p8 = " + p8)
  println("p7 + p8 = " + (p7 + p8))
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
    val b: Map[Int, anillo.R] = a.map(x => (x._1, anillo.field.build(x._2)))
    val c: anillo.T = anillo.Polynomial(b)
    c
  }

  def convert4(a: Map[Int, Int]) = {
    val anillo = anilloDePolis
    val b = a.map(x => (x._1, anilloDePolis.field.build(x._2)))
    val c = anilloDePolis.Polynomial(b)
    c
  }

  def convert5( p: Int, a: Map[Int, Int]) = {
    val cuerpo = Fp(p)
    val anillo = PolynomialsOverFp(cuerpo)
    val b = a.map(x => (x._1, anillo.field.build(x._2)))
    val c = anillo.Polynomial(b)
    c
  }





  println("Voy por aquí")



  val p = convert4(Map(3 -> 1, 2 -> 4))
  println(p, p.isIrreducible)

  val a = convert4(Map(5 -> 1))
  val b = convert4(Map(1 -> 4))

  println(a)
  println(b)
  val c = a + b
  println("a + b = " + (a+b))

  val q = convert5(5, Map(3 -> 1, 2 -> 4))
  println(q, q.isIrreducible)

  val aa = convert4(Map(5 -> 1))
  val bb = convert4(Map(1 -> 4))

  println(aa)
  println(bb)
  val cc = aa + bb
  println("aa + bb = " + (aa+bb))






  val cuerpo1 = Fp(5)
  val cuerpo2 = Fp(5)

  println(cuerpo1 == cuerpo2)

  val anillo1 = PolynomialsOverFp(cuerpo1)
  val anillo2 = PolynomialsOverFp(cuerpo2)

  println(anillo1 == anillo2)


















}

