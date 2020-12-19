package adventofcode

object Day18 {

  def parseEquation(string: String): (Long, Long, Char) = {
    val operator = if(string.contains('+')) '+' else '*'
    val nbs = string.split(operator)
    val nb1 = nbs.head.toLong
    val nb2 = nbs.last.toLong
    (nb1, nb2, operator)
  }

  def mathOperation(nb1: Long, nb2: Long, operator: Char): Long = operator match {
    case '+' => nb1 + nb2
    case '*' => nb1 * nb2
  }

  def computeMathLine(input: String): Long = {
    var line = input

    while(line.contains('(')){
      val subLineEnd = line.lastIndexOf(')')
      var nbParenthese = 1
      var iterator = 0
      while(nbParenthese > 0){
        iterator += 1
        if(line.charAt(subLineEnd - iterator) == ')')
          nbParenthese += 1
        else if (line.charAt(subLineEnd - iterator) == '(')
          nbParenthese -= 1
      }
      val result = computeMathLine(line.substring(subLineEnd - iterator + 1, subLineEnd))
      line = line.substring(0, subLineEnd - iterator) + result.toString + line.substring(subLineEnd+1)
      println(line)
    }

    if(line.count(_ == '+') + line.count(_ == '*') == 1){
      val (nb1, nb2, operator) = parseEquation(line)
      return mathOperation(nb1, nb2, operator)
    }

    val lastNb = line.split('+').last.split('*').last
    val operator = line.charAt(line.length - lastNb.length - 1)
    val result = computeMathLine(line.take(line.length - lastNb.length - 1))
    mathOperation(result, lastNb.toLong, operator)
  }

  def challenge(list: List[String]): Long = {
    computeMathLine(list.head.replace(" ", ""))
  }

  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour18.txt")
    println("Day 18 !")

    println("Challenge 1 : " + challenge(list))
    //println("Challenge 2 : " + challenge(list))

  }
}