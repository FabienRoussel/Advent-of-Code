package adventofcode

import scala.util.control.Breaks._

object Day16 {

  def extractMinsMaxs(str: String): List[(Int, Int)] = {
    val arrayMinsMaxs = str.trim.split(" or ")
    var listMinMax: List[(Int, Int)] = List[(Int, Int)]()
    arrayMinsMaxs.foreach(el => {
      val minAndMax: List[Int] = el.split('-').map(x => x.toInt).toList
      listMinMax = listMinMax :+ (minAndMax.head, minAndMax(1))
    })
    listMinMax
  }

  def extractData(list: List[String]): (Map[String, List[(Int, Int)]], List[Int], List[List[Int]]) = {
    var partOfExtractingData = 0

    var mapFields: Map[String, List[(Int, Int)]] = Map[String, List[(Int, Int)]]()
    var myTicket: List[Int] = List()
    var nearbyTickets: List[List[Int]] = List()

    list.foreach(el => {
      if (el == "your ticket:")
        partOfExtractingData = 1

      else if (el == "nearby tickets:") {
        partOfExtractingData = 2
      }
      if(el != "" && el != "your ticket:" && el != "nearby tickets:"){
        if(partOfExtractingData == 0){
          val data = el.split(":")
          val field = data(0)
          val listOfMinMax: List[(Int, Int)] = extractMinsMaxs(data(1))
          mapFields += (field -> listOfMinMax)
        }
        else if(partOfExtractingData == 1){
          myTicket = el.split(",").map(x => x.toInt).toList
        }
        else if(partOfExtractingData == 2){
          nearbyTickets = nearbyTickets :+ el.split(",").map(x => x.toInt).toList
        }
      }
    })
    (mapFields, myTicket, nearbyTickets)
  }

  def getInvalidTickets(nearbyTickets: List[List[Int]], mapFields: Map[String, List[(Int, Int)]]): List[Int] ={
    var invalidTickets: List[Int] = List()
    nearbyTickets.foreach( ticketList => {
      ticketList.foreach( ticket => {
        var isValid = false
        breakable { for ((_,v) <- mapFields) {
          v.foreach(minMax =>
            if(ticket >= minMax._1 && ticket <= minMax._2) {
              isValid = true
              break
            }
          )
        } }
        if(!isValid)
          invalidTickets = invalidTickets :+ ticket
      })
    })
    invalidTickets
  }

  def challenge1(list: List[String]): Int = {
    val (mapFields, _, nearbyTickets) = extractData(list)
    val invalidTickets: List[Int] = getInvalidTickets(nearbyTickets, mapFields)
    invalidTickets.sum
  }

  def getValidRows(nearbyTickets: List[List[Int]], invalidTickets: List[Int] = List()): List[List[Int]] = {
    var validNearbyTickets: List[List[Int]] = List()
    for(row <- nearbyTickets ){
      var containsOnlyValidTickets = true
      breakable {
        for(invalidTicket <- invalidTickets){
          if(row.contains(invalidTicket)){
            containsOnlyValidTickets = false
            break
          }
        }
        if(containsOnlyValidTickets)
          validNearbyTickets = validNearbyTickets :+ row
      }
    }
    validNearbyTickets
  }

  def ticketIsValidFor(field: String, ticket: Int, mapFields: Map[String, List[(Int, Int)]]): Boolean = {
    if(mapFields.contains(field))
      mapFields(field).foreach(minMax => { if(ticket >= minMax._1 && ticket <= minMax._2) return true })
    false
  }

  def removeUniqueFieldFromMap(mapFieldsReverse: Map[Int, List[String]]): Map[Int, List[String]] = {
    var newMapFieldsReverse: Map[Int, List[String]] = mapFieldsReverse
    var removedFieldsList: List[String] = mapFieldsReverse.values.toList.map(el => if(el.size==1) el.head else "").filter(_.nonEmpty)
    for(it <- 0 until mapFieldsReverse.size){
      val fieldToRemove = removedFieldsList(it)
      newMapFieldsReverse.foreach(fieldOfMap => {
        if(fieldOfMap._2.size > 1 && fieldOfMap._2.contains(fieldToRemove)) {
          val newList = fieldOfMap._2.filter(_!=fieldToRemove)
          newMapFieldsReverse += (fieldOfMap._1 -> newList)
          if (newList.size==1)
            removedFieldsList = removedFieldsList :+ newList.head
        }
      })
    }
    newMapFieldsReverse
  }

  def whichOrder(mapFields:           Map[String, List[(Int, Int)]],
                 validTicketsRows:    List[List[Int]]): Map[Int, List[String]] = {
    var mapFieldsReverse: Map[Int, List[String]] = Map[Int, List[String]]()
    val mapFieldsSize = mapFields.size
    for(it <- 0 until mapFieldsSize)
      mapFieldsReverse += (it -> mapFields.keys.toList)

    validTicketsRows.foreach(row => {
      for(it <- row.indices){
        val ticket = row(it)
        val fieldsList = mapFieldsReverse(it)

        if(fieldsList.size > 1){
          var newFieldsList: List[String] = List()
          fieldsList.foreach(field => {
            if(ticketIsValidFor(field=field, ticket=ticket, mapFields=mapFields)) newFieldsList = newFieldsList :+ field
          })
          mapFieldsReverse += (it -> newFieldsList)
        }
      }
    })
    mapFieldsReverse = removeUniqueFieldFromMap(mapFieldsReverse)
    mapFieldsReverse
  }

  def challenge2(list: List[String]): Long = {
    val (mapFields, myTicket, nearbyTickets) = extractData(list)
    val invalidTickets = getInvalidTickets(nearbyTickets, mapFields)
    val validTicketsRows = getValidRows(nearbyTickets, invalidTickets)
    val order: Map[Int, List[String]] = whichOrder(mapFields, validTicketsRows)

    var product: Long = 1
    for(it <- myTicket.indices){
      if(order(it).head.startsWith("departure")){
        product *= myTicket(it)
      }
    }
    product
  }

  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour16.txt")
    println("Day 16 !")

    println("Challenge 1 : " + challenge1(list))
    println("Challenge 2 : " + challenge2(list))
  }
}
