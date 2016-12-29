import scala.annotation.tailrec

object IsSorted {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(index: Int): Boolean = {
      if (index == as.length - 1) true
      else if ( !ordered( as(index), as(index + 1) ) )  false
      else loop(index + 1)
    }

    loop(0)
  }
}

object Main extends App {
  println(IsSorted.isSorted(Array(1,2,3,4), (a: Int, b: Int) => a < b)) // true
  println(IsSorted.isSorted(Array(6,8,1,5), (a: Int, b: Int) => a < b)) // false
}
