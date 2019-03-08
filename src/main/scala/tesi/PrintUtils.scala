package tesi

object PrintUtils {
  final val mask = 0xFFFFF
  def print(string: => String, i: Int): Unit =
    if ((i & mask) == mask) {
      System.out.print(string)
    }

  def println(string: => String, i: Int): Unit =
    if ((i & mask) == mask) {
      System.out.println(string)
    }

  def println(string: => String): Unit = System.out.println(string)
  def print(string: => String): Unit = System.out.print(string)
  def println(): Unit = System.out.println()
  def print(): Unit = System.out.print()
}
