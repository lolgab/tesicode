package tesi

import scala.collection.mutable.ArrayBuffer

object Splitter {
  def split(s: String, sep: Char): ArrayBuffer[String] = {
    var i = 0
    var currentStart = 0
    val buffer = new ArrayBuffer[String]()
    val length = s.length()
    while(i < length) {
      if(s(i) == sep) {
        buffer += s.substring(currentStart, i)
        currentStart = i + 1
      }
      i += 1
    }
    if(buffer.nonEmpty) buffer += s.substring(currentStart, i)
    buffer
  }
} 
