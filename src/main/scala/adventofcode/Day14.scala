package adventofcode

object Day14 {

  val MASK_SIZE = 36

  def extractMemoryAndValueToWrite(str: String): (Int, Long) = {
    val memoryInt: Int = str.takeRight(str.length - 4 ).split(']')(0).toInt
    val valueToWrite = str.split("= ").last.toLong
    (memoryInt, valueToWrite)
  }

  def computeNumberInMemory(mask: String, number: Long): String = {
    val binaryNum = number.toBinaryString
    var newNumber: String = mask.take(MASK_SIZE - binaryNum.length).replace('X', '0') + binaryNum

    for(it <- mask.indices){
      val char = if(mask(it) == 'X') newNumber(it) else mask(it)
      newNumber = newNumber.substring(0,it) + char + newNumber.substring(it+1)
    }

    newNumber
  }

  def challenge1(list: List[String]): Long = {
    var mask: String = ""
    var map: Map[Int, String] = Map[Int, String]()

    list.foreach(el => {
      if(el.take(4) == "mask") mask = el.takeRight(MASK_SIZE)
      else{
        val (memoryInt, valueToWrite) = extractMemoryAndValueToWrite(el)
        map += (memoryInt -> computeNumberInMemory(mask, valueToWrite))
      }
    })

    map.map(el => {
      var sum: Long = 0
      for(it <- el._2.indices){
        sum += Math.pow(2, MASK_SIZE - it - 1).toLong * el._2.charAt(it).asDigit
      }
      sum
    }).sum
  }

  def applyMask(mask: String, long: Long): String = {
    val memory = long.toBinaryString
    var address = "0" * (MASK_SIZE - memory.length) + memory

    for(it <- mask.indices){
      if(mask(it) == 'X') {
        address = address.substring(0,it) + 'X' + address.substring(it+1)
      } else {
        val char = mask(it).asDigit | address(it).asDigit
        address = address.substring(0,it) + char.toString + address.substring(it+1)
      }
    }

    address
  }

  def computeAdresses(address: String): List[String] = {
    var addresses: List[String] = List[String]()

    val nbX = address.count(_ == 'X')
    val maxXValue = Math.pow(2, nbX).toInt
    val zeroes: String = "0" * nbX

    for(it <- 0 until maxXValue){
      val itBinarySubString = it.toBinaryString
      val itBinary: String = zeroes.take(zeroes.length - itBinarySubString.length) + itBinarySubString
      var newAdress = address
      for(it2 <- 0 until nbX)
        newAdress = newAdress.replaceFirst("X", itBinary(it2).toString)
      addresses = addresses :+ newAdress
    }

    addresses
  }

  def writeMap(map: Map[String, Long], address: String, valueToWrite: Long): Map[String, Long] = {
    var newMap = map
    val adressesToWrite = computeAdresses(address)
    adressesToWrite.foreach(el => {
      newMap += (el -> valueToWrite)
    })
    newMap
  }

  def challenge2(list: List[String]): Long = {
    var mask: String = ""
    var map: Map[String, Long] = Map[String, Long]()

    list.foreach(el => {
      if(el.take(4) == "mask") mask = el.takeRight(36)
      else{
        val (memoryInt, valueToWrite) = extractMemoryAndValueToWrite(el)
        val address = applyMask(mask, memoryInt)
        map = writeMap(map, address, valueToWrite)
      }
    })

    map.values.sum
  }

  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour14.txt")
    println("Day 14 !")
    println("Challenge 1 : " + challenge1(list))
    println("Challenge 2 : " + challenge2(list))
  }
}
