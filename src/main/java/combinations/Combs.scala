package combinations

object Combs {

  def main(args: Array[String]): Unit = {

    def flatList[T](a: List[T]):List[List[T]] = {
      if(a == Nil) List(List())
      else a :: flatList(a.tail)
    }

    def combs[T](k: Int, a: List[T]): Unit = {

      def combsInner(k: Int, a:List[T], b: List[T]):Unit = {
        if(k == 0) {
          println(a)
        } else {
          flatList(b).foreach(x => {
            if(x.nonEmpty) {
              combsInner(k - 1, a :+ x.head, x.tail)
            }
          })
        }
      }

      combsInner(k, List(), a)
    }

    println(combs(3, List('a', 'b', 'c', 'd', 'e', 'f')).size)
  }
}
