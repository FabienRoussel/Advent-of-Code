package adventofcode

object Day12 {

  val DIRECTIONS_TO_INT: Map[Char, Int] = Map('N' -> 0, 'E' -> 1, 'S' -> 2, 'W' -> 3)
  val INT_TO_DIRECTIONS: Map[Int, Char] = Map(0 -> 'N', 1 -> 'E', 2 -> 'S', 3 -> 'W')
  val DIRECTIONS_TO_AXEX: Map[Char, Int] = Map('N' -> 0, 'E' -> 1, 'S' -> 0, 'W' -> -1)
  val DIRECTIONS_TO_AXEY: Map[Char, Int] = Map('N' -> -1, 'E' -> 0, 'S' -> 1, 'W' -> 0)

  def computeManhattanDistance(startingX: Int, startingY: Int, finishingX: Int, finishingY: Int): Int = {
    (finishingX - startingX).abs + (finishingY - startingY).abs
  }

  def extractInstruction(string: String): (Char, Int) = {
    val char = string.charAt(0)
    val int = string.takeRight(string.length-1).toInt
    (char, int)
  }

  def computeNewDirectionForLR(prevDir: Char, char: Char, int: Int): Char = {
    val update: Int = char match {
      case 'L' => -int / 90
      case 'R' => int / 90
      case _ => 0
    }
    INT_TO_DIRECTIONS( (update + DIRECTIONS_TO_INT(prevDir)  +4) % 4)
  }

  def computeNewPosition(posX: Int, posY: Int, dir: Char, instructChar: Char, instructInt: Int): (Int, Int) = {
    val (newPosX, newPosY): (Int, Int) = instructChar match {
      case 'F' => (posX + DIRECTIONS_TO_AXEX(dir)*instructInt, posY + DIRECTIONS_TO_AXEY(dir)*instructInt)
      case 'N' | 'S' | 'E' | 'W' => (posX + DIRECTIONS_TO_AXEX(instructChar)*instructInt, posY + DIRECTIONS_TO_AXEY(instructChar)*instructInt)
      case _ => (posX, posY)
    }
    (newPosX, newPosY)
  }

  def computePosition(posX: Int, posY: Int, dir: Char, instruction: String): (Int, Int, Char) = {
    val (instructChar, instructInt) = extractInstruction(instruction)
    val newDir = computeNewDirectionForLR(dir, instructChar, instructInt)
    val (newPosX, newPosY) = computeNewPosition(posX, posY, dir, instructChar, instructInt)

    (newPosX, newPosY, newDir)
  }

  def challenge1(): Int = {
    var posX = 0
    var posY = 0
    var dir = 'E'
    val file      = ReadFile.readFile("jour12.txt")

    file.foreach(el => {
      val (newPosX, newPosY, newDir) = computePosition(posX, posY, dir, el)
      posX = newPosX
      posY = newPosY
      dir = newDir
    })
    computeManhattanDistance(0, 0, posX, posY)
  }

  // PART II
  def computeNewDirectionForLRWaypoint(prevWaypointX: Int, prevWaypointY: Int, char: Char, int: Int): (Int, Int) = {
    val (newWaypointX, newWaypointY): (Int, Int) = char match {
      case 'L' =>
        int match {
          case 90 => (prevWaypointY, -prevWaypointX)
          case 180 => (-prevWaypointX, -prevWaypointY)
          case 270 => (-prevWaypointY, prevWaypointX)
        }
      case 'R' =>
        int match {
          case 90 => (-prevWaypointY, prevWaypointX)
          case 180 => (-prevWaypointX, -prevWaypointY)
          case 270 => (prevWaypointY, -prevWaypointX)
        }
      case 'N' | 'S' | 'E' | 'W' => (prevWaypointX + DIRECTIONS_TO_AXEX(char)*int, prevWaypointY + DIRECTIONS_TO_AXEY(char)*int)
      case _ => (prevWaypointX, prevWaypointY)
    }
    (newWaypointX, newWaypointY)
  }

  def computeNewPositionWaypoint(posX: Int, posY: Int, waypointX: Int, waypointY: Int, instructChar: Char, instructInt: Int): (Int, Int) = {
    val (newPosX, newPosY): (Int, Int) = instructChar match {
      case 'F' => (posX + waypointX * instructInt, posY +  waypointY * instructInt)
      case _ => (posX, posY)
    }
    (newPosX, newPosY)
  }

  def computePositionWaypoint(posX: Int, posY: Int, waypointX: Int, waypointY: Int, instruction: String): (Int, Int, Int, Int) = {
    val (instructChar, instructInt) = extractInstruction(instruction)
    val (newWaypointX, newWaypointY) = computeNewDirectionForLRWaypoint(waypointX, waypointY, instructChar, instructInt)
    val (newPosX, newPosY) = computeNewPositionWaypoint(posX, posY, newWaypointX, newWaypointY, instructChar, instructInt)

    (newPosX, newPosY, newWaypointX, newWaypointY)
  }

  def challenge2(): Int = {
    var posX = 0
    var posY = 0
    var waypointX = 10
    var waypointY = -1
    val file      = ReadFile.readFile("jour12.txt")

    file.foreach(el => {
      val (newPosX, newPosY, newWaypointX, newWaypointY) = computePositionWaypoint(posX, posY, waypointX, waypointY, el)
      posX = newPosX
      posY = newPosY
      waypointX = newWaypointX
      waypointY = newWaypointY
    })
    computeManhattanDistance(0, 0, posX, posY)
  }

  def main(args: Array[String]): Unit = {
    println("Day 12 !")
    println("Challenge 1 : " + challenge1())
    println("Challenge 1 : " + challenge2())
  }
}
