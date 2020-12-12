package aventofcode

object Day11 {

  var oldMatrix: List[String] = List[String]()
  var newMatrix: List[String] = List[String]()

  def countOccupiedSeatArroundTarget(targetRow: Int, targetCol: Int, matrix: List[String]): Int ={
    var count: Int = 0
    for(r <- -1 to 1){
      for(c <- -1 to 1){
        val row = targetRow + r
        val col = targetCol + c
        if( !(r == c  && r == 0 || row < 0 || row > matrix.size - 1 || col < 0 || col > matrix.head.length - 1)
            && matrix(row)(col) == '#' ){
          count += 1
        }
      }
    }
  count
  }

  def applySeatsRules(targetRow: Int, targetCol: Int, matrix: List[String]): Boolean = {
    val occupiedSeatsAdjacent       = countOccupiedSeatArroundTarget(targetRow, targetCol, matrix)
    var hasChanged = false

    occupiedSeatsAdjacent match {
      case 0 =>
        if(matrix(targetRow)(targetCol) == 'L' ){
          newMatrix = newMatrix.updated(targetRow, newMatrix(targetRow).substring(0, targetCol) + "#" + newMatrix(targetRow).substring(targetCol+1))
          hasChanged =  true
        }
      case i if i > 3 =>
        if(matrix(targetRow)(targetCol) == '#' ){
          newMatrix = newMatrix.updated(targetRow, newMatrix(targetRow).substring(0, targetCol) + "L" + newMatrix(targetRow).substring(targetCol+1))
          hasChanged = true
        }
      case _ => hasChanged = false
    }
    hasChanged
  }

  def challenge1(): Int = {
    newMatrix = ReadFile.readFile("jour11.txt")
    oldMatrix = newMatrix

    var hasChanged = true
    var security = 10000

    while(hasChanged && security > 0) {
      hasChanged = false
      for(row <- newMatrix.indices){
        for(col <- 0 until newMatrix(row).length){
          hasChanged = applySeatsRules(row, col, oldMatrix) || hasChanged
        }
      }
      oldMatrix = newMatrix
      security -= 1
    }

    var count = 0
    for(row <- oldMatrix.indices){
    count += oldMatrix(row).count(_ == '#')
    }
    count
  }

  def challenge2(): Long = {
    0
  }

  def main(args: Array[String]): Unit = {
    println("Day 10 !")
    println("Challenge 1 : " + challenge1())
    println("Challenge 2 : " + challenge2())
  }
}
