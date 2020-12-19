package adventofcode

import scala.io.Source

object Day9 {

    def readFile(): List[String] = {
        val bufferedSource              = Source.fromResource("jour9.txt")
        var content                     = List[String]()
        content = bufferedSource.getLines().toList
        bufferedSource.close()
        content
    }

    def isNumberTheSumOfTwoOthers(target: Long, list: List[Long]): Boolean = {
        val size: Int = list.size
        for(i <- 0 until size - 1){
            for(j <- i+1 until size){
                if(list(i) + list(j) == target){
                    return true
                }
            }
        }
        false
    }

    def findNumberNotTheSumOfOthers(preambleSize: Int = 25): Long = {
        val numbers: List[Long]       = readFile().map(_.toLong)
        val numberSize: Int = numbers.size

        for(i <- preambleSize until numberSize){
            if(!isNumberTheSumOfTwoOthers(numbers(i), numbers.slice(i-preambleSize, i))){
                return numbers(i)
            }
        }
        0
    }

    // PART II
    def findMinMaxOfListThatSumNumber(target: Long, list: List[Long]): (Long, Long) = {
        val size: Int = list.size
        for(i <- 0 until size - 1){
            var sum: Long = 0
            var min: Long = Long.MaxValue
            var max: Long = Long.MinValue
            for(j <- i until size){
                sum += list(j)
                if (list(j) > max)
                    max = list(j)
                if (list(j) < min)
                    min = list(j)
                if(sum == target){
                    return (min, max)
                }
            }
        }
        (0, 0)
    }

    def findNumberNotTheSumOfOthersChallenge2(preambleSize: Int = 25): Long = {
        val numbers: List[Long]       = readFile().map(_.toLong)
        val numberSize: Int = numbers.size

        for(i <- preambleSize until numberSize){
            if(!isNumberTheSumOfTwoOthers(numbers(i), numbers.slice(i-preambleSize, i))){
                val (min, max) = findMinMaxOfListThatSumNumber(numbers(i), numbers.take(i))
                return min + max
            }
        }
        0
    }

    def main(args: Array[String]): Unit = {
        println("Day 9 !")
        println(findNumberNotTheSumOfOthers())
        println(findNumberNotTheSumOfOthersChallenge2())
    }
}
