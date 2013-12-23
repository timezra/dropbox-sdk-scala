package timezra.dropbox.core

object Arrays {
  implicit def pimpArray[T: Manifest](array: Array[T]) = new PimpedArray(array)

  class PimpedArray[T: Manifest](val array: Array[T]) {
    def sliceT(from: Int, until: Int): Array[T] = {
      val lo = math max (from, 0)
      val hi = math min (math max (until, 0), array length)
      val elems = math max (hi - lo, 0)
      if (lo == 0 && hi == array.length) return array
      val newBuffer = new Array[T](elems)
      System arraycopy (array, lo, newBuffer, 0, elems)
      newBuffer
    }

    def takeT(n: Int) = sliceT(0, n)
  }
}
