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

  def getBusesWithIndex(busesList: List[String]): List[(Int, Int)] = {
    busesList.zipWithIndex.collect{ case x if x._1!="x" => (x._1.toInt, x._2)}
  }

  def getTimestampForTwoBusSynchro(departure: Long, bus1: Long, bus2: Int, shift: Int): Long ={
    var x = 0
    while((bus1 * x + departure + shift) % bus2 != 0)
      x += 1
    bus1 * x + departure
  }

  def findEarliestTimestamp(departure: Long, busesList: List[(Int, Int)]): Long ={
    if(busesList.size < 2)
      return 0
    val departureSynchro = findEarliestTimestamp(departure, busesList.take(busesList.size - 1))
    getTimestampForTwoBusSynchro(departureSynchro, busesList.take(busesList.size-1).map(x => x._1.toLong).product, busesList.last._1, busesList.last._2)
  }

  def challenge2(list: List[String]): Long = {
    val busesListWithDiff: List[(Int, Int)] = getBusesWithIndex(list(1).split(',').toList)
    findEarliestTimestamp(0, busesListWithDiff)
  }

  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour13.txt")
    println("Day 13 !")
    println("Challenge 1 : " + challenge1(list))
    println("Challenge 2 : " + challenge2(list))
  }
}
