import scala.collection.mutable

package object tesi {
  val Debug = true

  implicit class StrOps(val s: String) {
    def mySplit(sep: Char): Array[String] =
      if (s == "") Array.empty
      else s.split(sep)
  }

  final val Autore = 0
  final val Coautore = 1
  final val Cita = 2
  final val CitaComeCoautoreA = 3
  final val CitaComeCoautoreB = 4
  final val CitaComeCoautoreAAndB = 5
  final val ArrayRelazioneLength = 6

  type PesiType = Short

  type Matrice[T] = Array[Array[T]]
  type MatriceCreazione[T] = Array[mutable.ArrayBuilder[T]]
}
