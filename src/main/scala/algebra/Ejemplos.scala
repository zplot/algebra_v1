package algebra

import scala.annotation.tailrec

object Ejemplos {

// recursividad
  @tailrec
  private def suma(list: List[Int], accumulator: Int): Int = {
    list match {
      case Nil => accumulator
      case x :: xs => suma(xs, accumulator + x)
    }
  }


}
