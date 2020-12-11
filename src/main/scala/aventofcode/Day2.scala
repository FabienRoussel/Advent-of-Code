package aventofcode

import scala.io.Source

object Day2 {

    def readFile(): List[String] = {
        val bufferedSource              = Source.fromResource("jour2.txt")
        var content                     = List[String]()
        content = bufferedSource.getLines().toList
        bufferedSource.close()
        content
    }

    def main(args: Array[String]): Unit = {
        println("Day 2 !")
        print("Challenge 1 : ")
        println(countValidPasswordChallenge1())
        print("Challenge 2 : ")
        println(countValidPasswordChallenge2())
    }

    def countValidPasswordChallenge1(): Int = {
        val list: List[String]      = readFile()
        var count: Int              = 0
        var min: Int                = 0
        var max: Int                = 100000
        var char: Char              = ' '
        var password: String        = ""
        var decompose: Array[String] = Array()
        for (elementI <- list) {
            try {
                decompose = elementI.replace("-", " ").replace(":", "").split(" ")
                min = decompose(0).toInt
                max = decompose(1).toInt
                char = decompose(2).charAt(0)
                password = decompose(3)
                if (password.count(_ == char) >= min && password.count(_ == char) <= max) {
                    count += 1
                }
            } catch {
                case e: Exception => None
                    println("Exception")
            }
        }
        count
    }

    def countValidPasswordChallenge2(): Int = {
        val list: List[String]      = readFile()
        var count: Int              = 0
        var pos1: Int               = 0
        var pos2: Int               = 100000
        var char: Char              = ' '
        var password: String        = ""
        var decompose: Array[String] = Array()
        for (elementI <- list) {
            try {
                decompose = elementI.replace("-", " ").replace(":", "").split(" ")
                pos1 = decompose(0).toInt
                pos2 = decompose(1).toInt
                char = decompose(2).charAt(0)
                password = decompose(3)
                if (password(pos1 - 1) == char && password(pos2 - 1) != char
                  || password(pos1 - 1) != char && password(pos2 - 1) == char) {
                    count += 1
                }
            } catch {
                case e: Exception => None
                    println("Exception")
            }
        }
        count
    }
}
