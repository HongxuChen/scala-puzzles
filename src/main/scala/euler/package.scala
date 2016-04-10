

package object euler {

  def timeIt[A](block: => A, seconds: Int = 2, chunk: Int = 100, round: Int = 2): A = {
    var dummy: Any = 0
    for (i <- 0 to round) {
      val start = System.nanoTime()
      var counter = 0
      while (System.nanoTime() - start < seconds * 1e9) {
        var i = 0
        while (i < chunk) {
          dummy = block
          i += 1
        }
        counter += chunk
      }
      val duration = System.nanoTime() - start
      if (i != 0) {
        println(f"round=$i%d, counter=$counter%d, average=${duration.toDouble / counter}%.2fns")
      }
    }
    block
  }

}
