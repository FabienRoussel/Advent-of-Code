package adventofcode

import scala.annotation.tailrec
import java.util.regex.Pattern

object Day19 {

  def parseInput(list: List[String]): (Map[Int, List[String]], List[String]) = {
    var map: Map[Int, List[String]]= Map[Int, List[String]]()
    var messages: List[String] = List[String]()
    var parsingRules = true

    list.foreach( el => {
      if(parsingRules){
        if(el == "") parsingRules = false
        else {
          val rule = el.split(": ")
          val subRule: List[String] = rule.last.split('|').map(subRuleIdx => subRuleIdx.replaceAll("\"", "").trim).toList
          map += (rule.head.toInt -> subRule)
        }
      }
      else
        messages = messages :+ el
    })
    (map, messages)
  }

  def countValidMessages(validMessages: List[String], messages: List[String]): Int ={
    messages.count(msg => validMessages.contains(msg))
  }

  def replaceDigitInString(digit: Int, digitDecoded: List[String], encodedMessage: List[String]): List[String] = {
    var encodedMessageALittleLessEncoded: List[String] = List[String]()
    encodedMessage.foreach(msg => {
      digitDecoded.foreach(code =>{
        encodedMessageALittleLessEncoded = encodedMessageALittleLessEncoded :+ msg.replace(digit.toString, code)
      })
    })
    encodedMessageALittleLessEncoded
  }

  def isNumeric(str:String): Boolean = str.matches("[-+]?\\d+(\\.\\d+)?")

  def buildListOfValidMessages(map: Map[Int, List[String]], digit: Int, doneNodes: Set[Int]): (Map[Int, List[String]], Set[Int]) = {
    val messages = map(digit)
    var doneNodesItNext = doneNodes
    if(!map(digit).flatten.exists(_.isDigit)){
      (map, doneNodesItNext + digit)
    }
    else {
      var newMap = map
      val matcher = messages.mkString(" ").split(" ").distinct
      matcher.foreach(pointer => {
        if (isNumeric(pointer)){
          val digitChild = pointer.toInt
          if(doneNodesItNext.contains(digitChild)){
            val decodedMessage = replaceDigitInString(digitChild, newMap(digitChild), newMap(digit))
            newMap += (digit -> decodedMessage)
          }
          else{
            val (mapper, set) = buildListOfValidMessages(newMap, digitChild, doneNodesItNext)
            newMap = mapper
            val decodedMessage = replaceDigitInString(digitChild, newMap(digitChild), newMap(digit))
            doneNodesItNext = set
            newMap += (digit -> decodedMessage)
          }
        }
      })
      (newMap, doneNodesItNext)
    }
  }

  def challenge(map: Map[Int, List[String]], messages: List[String]): Int = {
    val mapDecoded = buildListOfValidMessages(map, 0, Set[Int]())
    println(mapDecoded)

    countValidMessages(mapDecoded._1(0).map(str => str.replaceAll("\\s", "")), messages)
  }

  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour19.txt")
    println("Day 19 !")
    val (map, messages) = parseInput(list)
    println("Challenge 1 : " + challenge(map, messages))
  }
}