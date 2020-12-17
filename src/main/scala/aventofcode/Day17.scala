package aventofcode

object Day17 {

  def printSpace(space: List[List[List[Char]]]) = {
    space.foreach( matrice =>
      matrice.foreach(row => {
        row.foreach( cube => print(cube))
        println()
      })
    )
    println()
  }

  def computeCubeInActiveState(space: List[List[List[Char]]]): Int = {
    space.flatten.flatten.count(_=='#')
  }

  def formatInput(list: List[String]): List[List[List[Char]]] = {
    val outputSpace: List[List[Char]] = list.map(x => x.toCharArray.toList)
    List(outputSpace)
  }

  def expandSpace(space: List[List[List[Char]]]): List[List[List[Char]]] = {
    val expandedSpace = space.map(z => List.fill(space.head.size+2)('.') :: z.map(y => '.' :: y ::: List('.')) ::: List(List.fill(space.head.size+2)('.')))
    List.fill(expandedSpace.head.size)(List.fill(expandedSpace.head.head.size)('.')) :: expandedSpace ::: List(List.fill(expandedSpace.head.size)(List.fill(expandedSpace.head.head.size)('.')))
  }

  def computeNewCubeState(posX: Int, posY: Int, posZ: Int, space: List[List[List[Char]]]): Char= {
    val minZ = posZ - 1
    val maxZ = posZ + 1
    val minY = posY - 1
    val maxY = posY + 1
    val minX = posX - 1
    val maxX = posX + 1
    val currentPosStatus = space(posZ)(posX)(posY)
    val subSpace = space.slice(minZ, maxZ + 1).map(column => column.slice(minX, maxX + 1).map(row => row.slice(minY, maxY + 1)))
    val count = computeCubeInActiveState(subSpace) - (if( currentPosStatus == '#') 1 else 0)
    count match {
      case 2 | 3 if currentPosStatus == '#' => '#'
      case 3 if currentPosStatus == '.' => '#'
      case _ => '.'
    }
  }

  def computeNewSpaceState(space: List[List[List[Char]]]): List[List[List[Char]]] = {
    val expandedSpace = expandSpace(space)
    val updatedExpandedSpace = expandedSpace.zipWithIndex.map(
      matrice => matrice._1.zipWithIndex.map(
        row => row._1.zipWithIndex.map(cube => computeNewCubeState(row._2, cube._2, matrice._2, expandedSpace))))
    updatedExpandedSpace
  }

  def challenge1(space: List[List[List[Char]]]): Int = {
    var explodingSpace = space
    for(_ <- 0 until 6){
      explodingSpace = computeNewSpaceState(explodingSpace)
    }

    computeCubeInActiveState(explodingSpace)
  }

  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour17.txt")
    val space = formatInput(list)
    println("Day 17 !")

    println("Challenge 1 : " + challenge1(space))
    println(computeNewSpaceState(space))
  }
}