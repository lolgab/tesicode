package tesi

object TimeUtils {
  def timeInfo[T](message: String)(f: => T): T = {
    val time = System.currentTimeMillis()
    val res = f
    println(s"$message: took ${System.currentTimeMillis() - time} millis")
    res
  }
}
