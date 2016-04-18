package s99

class TArithmetic extends BaseSpec {

  import SArithmetic._

  "isPrime" should "Determine whether a given integer number is prime." in {
    1.isPrime should be(false)
    2.isPrime should be(true)
    7.isPrime should be(true)
    9.isPrime should be(false)
  }

  "gcd" should "Determine the greatest common divisor of two positive integer numbers." in {
    gcd(36, 63) should be(9)
    gcd(36, 35) should be(1)
  }

  "isCoprimeTo" should "Determine whether two positive integer numbers are coprime." in {
    35.isCoprimeTo(64) should be(true)
  }

  "totient" should "Calculate Euler's totient function phi(m)." in {
    10.totient should be(4)
    9.totient should be(6)
  }

  "primeFactors" should "Determine the prime factors of a given positive integer." in {
    17.primeFactors should be(List(17))
    10.primeFactors should be(List(2, 5))
    315.primeFactors should be(List(3, 3, 5, 7))
  }

  "primeFactorMultiplicity" should "Determine the prime factors of a given positive integer (2)." in {
    17.primeFactorMultiplicity should be(Map(17 -> 1))
    17.primeFactorMultiplicity2 should be(List(17 -> 1))
    10.primeFactorMultiplicity should be(Map(2 -> 1, 5 -> 1))
    10.primeFactorMultiplicity2 should be(List((2, 1), (5, 1)))
    315.primeFactorMultiplicity should be(Map(3 -> 2, 5 -> 1, 7 -> 1))
    315.primeFactorMultiplicity2 should be(List((3, 2), (5, 1), (7, 1)))
  }

  "totientImproved" should "Calculate Euler's totient function phi(m) (improved)." in {
    10.totientImproved should be(10.totient)
    9.totientImproved should be(9.totient)
  }

  "compareTotient" should "Compare the two methods of calculating Euler's totient function." in {
    val (r1, r2) = compareTotient(10090)
    r1._1 should be(r1._1)
    r1._2 should be > r2._2
  }

  "listPrimesinRange" should "return A list of prime numbers." in {
    listPrimesinRange(7, 31) should be(List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  "goldbach" should "deals with Goldbach's conjecture." in {
    28.goldbach should be((5, 23))
    30.goldbach should be((7, 23))
  }

  "printGoldbachList" should "generates A list of Goldbach compositions." in {
    printGoldbachList(9, 20) should be(List((10, (3, 7)), (12, (5, 7)), (14, (3, 11)), (16, (3, 13)), (18, (5, 13)), (20, (3, 17))))
    printGoldbachListLimited(1, 2000, 50) should have length 4
  }
}
