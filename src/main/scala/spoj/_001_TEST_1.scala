package spoj

import scala.io.Source

object _001_TEST_1 extends App {
  var a = Source.stdin.toString().toInt
  println(a)
  while (a != 42) {
    println(a)
    a = Source.stdin.toString.toInt
  }
}
