package aventofcode

import scala.io.Source

object Day5 {

    def readFile(): List[String] = {
        val bufferedSource              = Source.fromResource("jour5.txt")
        var content                     = List[String]()
        content = bufferedSource.getLines().toList
        bufferedSource.close()
        content
    }

    def searchHigherSeatID(): Int = {
        var highestSeatId: Int          = 0
        val seatsList: List[String]     = readFile()
        var row: Int = 0
        var col: Int = 0
        seatsList.foreach(el => {
            row = Integer.parseInt(el.take(7).replace('F', '0').replace('B', '1'), 2)
            col = Integer.parseInt(el.takeRight(3).replace('L', '0').replace('R', '1'), 2)
            if(highestSeatId < row * 8 + col)
                highestSeatId = row * 8 + col
        })
        highestSeatId
    }

    def searchMissingSeatID(): Long = {
        val seatsList: List[String]     = readFile()
        var seatsListInId: Array[Int]   = Array()

        seatsList.foreach(el => {
            val row: Int = Integer.parseInt(el.take(7).replace('F', '0').replace('B', '1'), 2)
            val col: Int = Integer.parseInt(el.takeRight(3).replace('L', '0').replace('R', '1'), 2)
            seatsListInId :+= row * 8 + col
        })

        seatsListInId = seatsListInId.sorted
        var missingSeat: Long = (seatsListInId.length + 1) * (seatsListInId(0) + seatsListInId(seatsListInId.length-1)) / 2
        seatsListInId.foreach(el => {
            missingSeat -= el
        })

        missingSeat
    }

    def main(args: Array[String]): Unit = {
        println("Day 5 !")
        //println(searchHigherSeatID())
        println(searchMissingSeatID())
    }
}