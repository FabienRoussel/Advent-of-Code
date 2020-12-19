package adventofcode

import starterkit.AbstractSpec

class Day19Spec extends AbstractSpec {
  "My test" when {
    "countValidMessages is called with two list" should {
      "return the 2" in {
        // Given
        val validMessages = List("aaaabb", "aaabab", "abbabb", "abbbab", "aabaab", "abaaab", "aabbbb", "ababbb")
        val messages = List("ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb")

        // When
        val actualCount = Day19.countValidMessages(validMessages, messages)

        // Then
        val expected = 2
        actualCount shouldBe expected
      }
    }

    "buildListOfValidMessages is called with map" should {
      "return the list of valid messages" in {
        val map: Map[Int, List[String]] = Map[Int, List[String]](
          0 -> List("46 1 5"),
          1 -> List("2 3", "3 2"),
          2 -> List("46 46", "5 5"),
          3 -> List("46 5", "5 46"),
          46 -> List("a"),
          5 -> List("b"))

        // When
        val actual = Day19.buildListOfValidMessages(map, 0, Set[Int]())

        // Then
        val expected = Map[Int, List[String]](
          0 -> List("a a a a b b", "a a a b a b", "a b b a b b", "a b b b a b", "a a b a a b", "a b a a a b", "a a b b b b", "a b a b b b"),
          1 -> List("a a a b", "a a b a", "b b a b", "b b b a", "a b a a", "b a a a", "a b b b", "b a b b"),
          2 -> List("a a", "b b"),
          3 -> List("a b", "b a"),
          46 -> List("a"),
          5 -> List("b"))

        actual._1 shouldBe expected
      }
    }

    "buildListOfValidMessages is called with map for digit 3" should {
      "return the list of valid messages" in {
        val map: Map[Int, List[String]] = Map[Int, List[String]](
          0 -> List("4 1 5"),
          1 -> List("2 3", "3 2"),
          2 -> List("4 4", "5 5"),
          3 -> List("4 5", "5 4"),
          4 -> List("a"),
          5 -> List("b"))

        // When
        val actual = Day19.buildListOfValidMessages(map, 3, Set[Int]())

        // Then
        val expected = Map[Int, List[String]](
          0 -> List("4 1 5"),
          1 -> List("2 3", "3 2"),
          2 -> List("4 4", "5 5"),
          3 -> List("a b", "b a"),
          4 -> List("a"),
          5 -> List("b"))

        actual._1 shouldBe expected
      }
    }

    "buildListOfValidMessages is called with map and already decoded message" should {
      "return the list of valid messages" in {
        // Given
        val map: Map[Int, List[String]] = Map[Int, List[String]](
          0 -> List("4 1 5"),
          1 -> List("2 3", "3 2"),
          2 -> List("4 4", "5 5"),
          3 -> List("4 5", "5 4"),
          4 -> List("a"),
          5 -> List("b"))

        // When
        val actual = Day19.buildListOfValidMessages(map, 4, Set[Int]())

        // Then
        val expected = Map[Int, List[String]](
          0 -> List("4 1 5"),
          1 -> List("2 3", "3 2"),
          2 -> List("4 4", "5 5"),
          3 -> List("4 5", "5 4"),
          4 -> List("a"),
          5 -> List("b"))
        actual._1 shouldBe expected
      }
    }

    "replaceDigitInString is called with digit 1 and string a15" should {
      "return list a235 and a325" in {
        // Given
        val digit             = 1
        val digitDecoded      = List("2 3", "3 2")
        val encodedMessage    = List("a 1 5")

        // When
        val actual = Day19.replaceDigitInString(digit, digitDecoded, encodedMessage)

        // Then
        val expected = List("a 2 3 5", "a 3 2 5")
        actual shouldBe expected
      }
    }

    "replaceDigitInString is called with string and decodedDigit" should {
      "return list of decoded string" in {
        // Given
        val digit             = 2
        val digitDecoded      = List("4 4", "5 5")
        val encodedMessage    = List("a 2 3 5", "a 3 2 5")

        // When
        val actual = Day19.replaceDigitInString(digit, digitDecoded, encodedMessage)

        // Then
        val expected = List("a 4 4 3 5", "a 5 5 3 5", "a 3 4 4 5", "a 3 5 5 5")
        actual shouldBe expected
      }
    }
  }
}
