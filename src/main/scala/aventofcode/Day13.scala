package aventofcode

object Day13 {

  def extractData(list: List[String]):(Long, List[Int]) = {
    val arrivalTime = list.head.toInt
    val buses = list(1).replace("x,", "").split(',').map(_.toInt).toList
    (arrivalTime, buses)
  }

  def getWaitTime(arrivalTime: Long, bus: Int): Long = bus - arrivalTime % bus

  def getEarliestBus(arrivalTime: Long, busesList: List[Int]): Int ={
    var busTaken = 0
    var minimalWaitTime: Long = busesList.max
    busesList.foreach(bus =>{
      val waitingTime = getWaitTime(arrivalTime, bus)
      if( waitingTime < minimalWaitTime){
        minimalWaitTime = waitingTime
        busTaken = bus
      }
    })
    busTaken
  }

  def challenge1(list: List[String]): Long = {
    val (arrivalTime, busesList) = extractData(list)

    val earliestBus = getEarliestBus(arrivalTime, busesList)
    earliestBus * getWaitTime(arrivalTime, earliestBus)
  }


  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour13.txt")
    println("Day 13 !")
    println("Challenge 1 : " + challenge1(list))
  }
}
