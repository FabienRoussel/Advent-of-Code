package adventofcode

import scala.io.Source

object Day3 {

    def main(args: Array[String]): Unit = {
        println("Day 3 !")
        println("Challenge 1 : " + j3TreeEncounterChallenge1(slopes = 3, pass = false))
        println("Challenge 2 : " + j3TreeEncounterChallenge2())
    }

    def j3TreeEncounterChallenge1(slopes: Int, pass: Boolean): Int = {
        val bufferedSource              = Source.fromResource("jour3.txt")
        var forest                      = List[String]()
        for (line <- bufferedSource.getLines) {
            forest ::= line
        }
        bufferedSource.close
        forest = forest.reverse

        var treeEncounter               = 0
        var previous_row_passed: Boolean = false
        val lineLength: Int             = forest.head.length
        var cursorIndex: Int            = 0
        for(line <- forest){
            if(pass && previous_row_passed){
                previous_row_passed = false
            }
            else {
                if (line(cursorIndex) == '#') {
                    treeEncounter += 1
                }

                cursorIndex += slopes
                if (cursorIndex > lineLength - 1) {
                    cursorIndex -= lineLength
                }
                previous_row_passed = true
            }
        }
        treeEncounter
    }

    def j3TreeEncounterChallenge2(): Long = {
        var multiple: Long = 1
        for (slope: Int <- List(1, 3, 5, 7)) {
            multiple = multiple * j3TreeEncounterChallenge1(slope, pass = false)
        }
        multiple * j3TreeEncounterChallenge1(slopes = 1, pass = true)
    }
}

