package aventofcode

import scala.collection.mutable.Queue
import scala.io.Source
import scala.util.control.Breaks.break

object Day8 {

    def readFile(): List[String] = {
        val bufferedSource              = Source.fromResource("jour8.txt")
        var content                     = List[String]()
        content = bufferedSource.getLines().toList
        bufferedSource.close()
        content
    }

    def getInstruction(str: String): (String, Int) = {
        val instruction: String = str.take(3)
        val value: Int          = if(str(4)=='+') str.slice(5, str.length).toInt else -str.slice(5, str.length).toInt
        (instruction, value)
    }

    def computeAccumulator(instructionList: List[String] = readFile()): Option[Int] = {
        var accumulator: Int                    = 0
        val instructionListSize: Int            = instructionList.size
        val visitedInstruction: Array[Boolean]  = Array.fill(instructionListSize)(false)
        var i: Int = 0

        while(i < instructionListSize){
            if(!visitedInstruction(i)){
                visitedInstruction(i) = true
                val (instruction, value): (String, Int) = getInstruction(instructionList(i))
                instruction match {
                    case "acc"  => accumulator += value
                    case "jmp"  => i += value-1
                    case _      =>
                }
                i += 1
            }
            else {
                i = instructionListSize
                return None
            }
        }
        Some(accumulator)
    }

    // PART II
    def findAccValueForWhichItWorks(): Int = {
        val instructionList: List[String]       = readFile()
        var instructionListCopy: List[String]   = instructionList
        val instructionListSize: Int            = instructionList.size
        var accumulator: Option[Int]            = None
        var it: Int                             = 0

        while(it < instructionListSize) {
            val line = instructionList(it)
            val (instruction, _): (String, Int) = getInstruction(line)
            if (instruction == "jmp"){
                instructionListCopy = instructionList.updated(it, line.replace("jmp", "nop"))
                accumulator = computeAccumulator(instructionListCopy)
            }
            else if (instruction == "nop"){
                instructionListCopy = instructionList.updated(it, line.replace("nop", "jmp"))
                accumulator = computeAccumulator(instructionListCopy)
            }
            if(accumulator.isDefined)
                it = instructionListSize
            it += 1
        }
        accumulator.get
    }

    def main(args: Array[String]): Unit = {
        println("Day 8 !")
        println(computeAccumulator())
        println(findAccValueForWhichItWorks())
    }
}
