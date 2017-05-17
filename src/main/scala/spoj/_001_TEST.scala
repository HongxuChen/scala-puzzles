package spoj

import scala.io.StdIn

object _001_TEST extends App {
  Iterator.continually(StdIn.readInt()).takeWhile(_ != 42).foreach(println)
}
