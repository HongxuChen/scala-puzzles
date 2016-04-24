package utils


object Bench {

  def timeIt[A](block: => A, seconds: Int, chunk: Int, round: Int): A = {
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
        println(f"time=$seconds%ds, round=$i%d, counter=$counter%d, average=${duration.toDouble / counter / 1e6}%.4fms")
      }
    }
    dummy.asInstanceOf[A]
  }

  def pTimeIt[A](block: => A, seconds: Int = 1, chunk: Int = 100, round: Int = 2): Unit = {
    val res = timeIt(block, seconds, chunk, round)
    println(s"res=$res")
  }

}
