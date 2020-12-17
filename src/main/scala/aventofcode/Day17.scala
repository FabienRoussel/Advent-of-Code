package aventofcode

object Day17 {

  def formatInput(list: List[String]): List[List[List[Char]]] = {
    List(list.map(x => x.toCharArray.toList))
  }

  def computeCubeInActiveState(space: List[List[List[Char]]]): Int = { space.flatten.flatten.count(_=='#') }
  def computeCubeInActiveState4D(space: List[List[List[List[Char]]]]): Int = { space.flatten.flatten.flatten.count(_=='#') }

  def expandSpace(space: List[List[List[Char]]]): List[List[List[Char]]] = {
    val expandedSpace = space.map(z => List.fill(space.head.size+2)('.') :: z.map(y => '.' :: y ::: List('.')) ::: List(List.fill(space.head.size+2)('.')))
    val emptyMatrix = List.fill(expandedSpace.head.size)(List.fill(expandedSpace.head.head.size)('.'))
    emptyMatrix :: expandedSpace ::: List(emptyMatrix)
  }

  def expandSpace4D(space4D: List[List[List[List[Char]]]]): List[List[List[List[Char]]]] = {
    val expandedSpace = space4D.map(space3D => expandSpace(space3D))
    val emptySpace3D = List.fill(expandedSpace.head.size)(List.fill(expandedSpace.head.head.size)(List.fill(expandedSpace.head.head.head.size)('.')))
    emptySpace3D :: expandedSpace ::: List(emptySpace3D)
  }

  private def newStateFromCount(currentPosStatus: Char, count: Int): Char = {
    count match {
      case 2 | 3 if currentPosStatus == '#' => '#'
      case 3 if currentPosStatus == '.' => '#'
      case _ => '.'
    }
  }

  def computeNewCubeState(posX: Int, posY: Int, posZ: Int, space: List[List[List[Char]]]): Char= {
    val currentPosStatus = space(posZ)(posX)(posY)
    val subSpace = space.slice(posZ - 1, posZ + 2)
      .map(column   => column.slice(posX - 1, posX + 2)
        .map(row    => row.slice(posY - 1, posY + 2)))
    val count = computeCubeInActiveState(subSpace) - (if( currentPosStatus == '#') 1 else 0)
    newStateFromCount(currentPosStatus, count)
  }

  def computeNewCubeState4D(posX: Int, posY: Int, posZ: Int, posW: Int, space4D: List[List[List[List[Char]]]]): Char= {
    val currentPosStatus = space4D(posW)(posZ)(posX)(posY)
    val subSpace4D = space4D.slice(posW - 1, posW + 2)
      .map(space    => space.slice(posZ - 1, posZ + 2)
        .map(column => column.slice(posX - 1, posX + 2)
          .map(row  => row.slice(posY - 1, posY + 2))))
    val count = computeCubeInActiveState4D(subSpace4D) - (if( currentPosStatus == '#') 1 else 0)
    newStateFromCount(currentPosStatus, count)
  }

  def computeNewSpaceState(space: List[List[List[Char]]]): List[List[List[Char]]] = {
    val expandedSpace = expandSpace(space)
    val updatedExpandedSpace = expandedSpace.zipWithIndex.map(
      matrice => matrice._1.zipWithIndex.map(
        row => row._1.zipWithIndex.map(cube => computeNewCubeState(row._2, cube._2, matrice._2, expandedSpace))))
    updatedExpandedSpace
  }

  def computeNewSpaceState4D(space4D: List[List[List[List[Char]]]]): List[List[List[List[Char]]]] = {
    val expandedSpace4D: List[List[List[List[Char]]]] = expandSpace4D(space4D)
    val updatedExpandedSpace = expandedSpace4D.zipWithIndex.map( space =>
      space._1.zipWithIndex.map(
        matrice => matrice._1.zipWithIndex.map(
          row => row._1.zipWithIndex.map(cube => computeNewCubeState4D(row._2, cube._2, matrice._2, space._2, expandedSpace4D)))))
    updatedExpandedSpace
  }

  def challenge(space: List[List[List[Char]]], dimension: Int): Int = {
    if(dimension == 3) {
      var explodingSpace = space
      for(_ <- 0 until 6){
        explodingSpace = computeNewSpaceState(explodingSpace)
      }
      return computeCubeInActiveState(explodingSpace)
    }
    else if(dimension == 4) {
      var explodingSpace = List(space)
      for(_ <- 0 until 6){
        explodingSpace = computeNewSpaceState4D(explodingSpace)
      }
      return computeCubeInActiveState4D(explodingSpace)
    }
    0
  }

  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour17.txt")
    val space = formatInput(list)
    println("Day 17 !")

    println("Challenge 1 : " + challenge(space, dimension=3))
    println("Challenge 2 : " + challenge(space, dimension=4))
  }
}