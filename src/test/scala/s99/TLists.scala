package s99

class TLists extends BaseSpec {

  val l1 = List(1, 1, 2, 3, 5, 8)
  val l2 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  import SLists.{length => sLength, _}

  "last" should "Find the last element of a list." in {
    last(l1) should be(l1.last)
    intercept[IndexOutOfBoundsException] {
      last(Nil)
    }
  }

  "penultimate" should "Find the last but one element of a list." in {
    penultimate(l1) should be(5)
    for (l <- List(List("str"), Nil)) {
      intercept[IndexOutOfBoundsException] {
        penultimate(l)
      }
    }
  }

  "nth" should "Find the Kth element of a list." in {
    nth(2, l1) should be(l1(2))
    nth(0, l1) should be(l1.head)
    for (i <- List(-1, 10)) {
      intercept[IndexOutOfBoundsException] {
        nth(i, l1)
      }
    }
  }

  "length" should "Find the number of elements of a list." in {
    sLength(Nil) should be(0)
    sLength(l1) should be(l1.length)
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
    duplicate(Nil) should be(Nil)
    duplicate(List(Nil)) should be(List(Nil, Nil))
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
    val ll = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    slice(3, 7, ll) should be(List('d, 'e, 'f, 'g))
    slice(-1, 100, ll) should be(ll)
  }

  "rotate" should "Rotate a list N places to the left." in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  "removeAt" should "Remove the Kth element from a list." in {
    val ll = List('a, 'b, 'c, 'd)
    removeAt(1, ll) should be((List('a, 'c, 'd), 'b))
    removeAt(0, ll) should be((List('b, 'c, 'd), 'a))
  }

  "insertAt" should "Insert an element at a given position into a list." in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be(List('a, 'new, 'b, 'c, 'd))
  }

  "range" should "Create a list containing all integers within a given range." in {
    "range('a, 'c)" shouldNot compile
    range(4, 9) should be(List(4, 5, 6, 7, 8, 9))
  }

  "randomSelect" should "Extract a given number of randomly selected elements from a list." in {
    val inList = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val res = randomSelect(3, inList)
    res should have length 3
    res.forall(inList.contains(_)) should be(true)
  }

  "lotto" should "Lotto: Draw N different random numbers from the set 1..M." in {
    val num = 6
    val max = 49
    val res = lotto(num, max)
    res should have length num
    res.forall(e => e <= max && e >= 0)
  }

  "randomPermute" should "Generate a random permutation of the elements of a list." in {
    val inList = List('a, 'b, 'c, 'd, 'e, 'f)
    val outList = randomPermute(inList)
    inList.sortBy(_.name) == outList.sortBy(_.name) should be(true)
  }

  "combinations" should "Generate the combinations of K distinct objects chosen from the N elements of a list." in {
    val l = List('a, 'b, 'c, 'd, 'e, 'f)
    val builtinCombinations = l.combinations(3).toList
    val comb = combinations(3, l)
    comb should have length comb.length
    comb.forall(builtinCombinations.contains(_)) should be(true)
    combinations(6, l) should be(List(l))
    comb shouldBe a[List[_]]
  }

  "group3" should "Group the elements of a set into disjoint subsets." ignore {
    val l = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val res = group(List(2, 2, 5), l)
    res.length should be(1260)
  }

  "lsort lsortFreq" should "Sorting a list of lists according to length of sublists." in {
    val l = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    lsort(l) should be(List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
    lsortFreq(l) should be(List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
  }


}
