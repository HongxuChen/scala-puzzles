package s99

class TLogic extends BaseSpec {

  import SLogic._

  val input = List((true, true), (true, false), (false, true), (false, false))
  val andTruth = List(true, false, false, false)
  val orTruth = List(true, true, true, false)
  val xorTruth = List(false, true, true, false)
  val implTruth = List(true, false, true, true)

  "truth" should "denotes truth tables for logical expressions." in {
    def run(f: (Boolean, Boolean) => Boolean, l: List[(Boolean, Boolean)]) = l.map(t => f.tupled(t))

    run(and, input) should be(andTruth)
    run(or, input) should be(orTruth)
    run(nand, input) should be(andTruth.map(!_))
    run(nor, input) should be(orTruth.map(!_))
    run(xor, input) should be(xorTruth)
    run(impl, input) should be(implTruth)
    run(equ, input) should be(xorTruth.map(!_))

    table(equ)
    table((a: Boolean, b: Boolean) => and(a, or(a, b)))
  }

  "gray" should "generates Gray Code." in {
    gray(3) should be(List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  "huffman" should "Huffman code." ignore {
    huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) should be(List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100")))
  }

}
