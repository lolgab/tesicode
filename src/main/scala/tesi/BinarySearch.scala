package tesi

object BinarySearch {
  def binarySearch[@specialized(Int) T](array: Array[T], value: T)(
      implicit ordering: Ordering[T]): Int = {
    import ordering._
    var low = 0
    var high = array.length - 1

    while (low <= high) {
      val mid = low + (high - low) / 2
      val midValue = array(mid)
      if (value < midValue) {
        high = mid - 1
      } else if (value > midValue) {
        low = mid + 1
      } else {
        return mid
      }
    }
    -1
  }
}
