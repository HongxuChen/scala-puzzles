package s99

import org.scalatest.words.{MatcherWords => _}
import org.scalatest.{words => _, _}


class TLists extends FlatSpec with BeforeAndAfterAll with Matchers with ParallelTestExecution {

  val l1 = List(1, 1, 2, 3, 5, 8)
  val l2 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  import SLists.{length => sLength, _}

  "last" should "Find the last element of a list." in {
    last(l1) should be(8)
    a[NoSuchElementException] should be thrownBy last(Nil)
  }

  "penultimate" should "Find the last but one element of a list." in {
    penultimate(l1) should be(5)
    for (l <- List(List("str"), Nil)) {
      a[NoSuchElementException] should be thrownBy penultimate(l)
    }
  }

  "nth" should "Find the Kth element of a list." in {
    nth(2, l1) should be(2)
    nth(0, l1) should be(1)
    for (i <- List(-1, 10)) {
      a[IndexOutOfBoundsException] should be thrownBy nth(i, l1)
    }
  }

  "length" should "Find the number of elements of a list." in {
    sLength(Nil) should be(0)
    sLength(l1) should be(6)
  }

  "reverse" should "Reverse a list." in {
    reverse(Nil) should be(Nil)
    reverse(l1) should be(l1.reverse)
  }

  "isPalindrome" should "Find out whether a list is a palindrome." in {
    isPalindrome(Nil) should be(true)
    isPalindrome(l1) should be(false)
    isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
  }

  "flatten" should "Flatten a nested list structure." in {
    flatten(Nil) should be(Nil)
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(l1)
    flatten(List(Nil, List("String", 'c'), List(List(1), List(8L, 3.0d)))) should be(List("String", 'c', 1, 8L, 3.0d))
  }

  "compress" should "Eliminate consecutive duplicates of list elements." in {
    compress(Nil) should be(Nil)
    compress(l2) should be(List('a, 'b, 'c, 'a, 'd, 'e))
    compress(List("str")) should be(List("str"))
  }

  "pack" should "Pack consecutive duplicates of list elements into sublists." in {
    pack(Nil) should be(Nil)
    pack(l2) should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    pack(List(Nil, 'a, 'b, 'c, 'c)) should be(List(List(Nil), List('a), List('b), List('c, 'c)))
    pack(List('a)) should be(List(List('a)))
    pack(List('b, 'a)) should be(List(List('b), List('a)))
  }

  "encode" should "Run-length encoding of a list." in {
    encode(Nil) should be(Nil)
    encode(l2) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "encodeModified" should "Modified run-length encoding." in {
    encodeModified(Nil) should be(Nil)
    encodeModified(l2) should be(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  "decode" should "Decode a run-length encoded list." in {
    decode(Nil) should be(Nil)
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "encodeDirect" should "Run-length encoding of a list." in {
    encodeDirect(Nil) should be(Nil)
    encodeDirect(l2) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "duplicate" should "Duplicate the elements of a list." in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "duplicateN" should "Duplicate the elements of a list a given number of times." in {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  "drop" should "Drop every Nth element from a list." in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "split" should "Split a list into two parts." in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  "slice" should "Extract a slice from a list." in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g))
  }

}
