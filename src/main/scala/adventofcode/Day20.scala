package adventofcode

object Day20 {

  def parseInput(list: List[String]): List[(Int, List[String])] = {
    var tiles: List[(Int, List[String])] = List[(Int, List[String])]()
    var tileId: Int = 0
    var tilePhoto: List[String] = List()
    (list :+ "").foreach(el => {
      if(el.startsWith("Tile")){
        val id = el.split(' ').last
        tileId = id.take(id.length - 1).toInt
      }
      else if(el != ""){
        tilePhoto = tilePhoto :+ el
      }
      else{
        tiles = tiles :+ (tileId, tilePhoto)
        tilePhoto = List()
      }
    })
    tiles
  }

  def getBordersNESW(tile: List[String]): (String, String, String, String) = {
    val north = tile.head
    val south = tile.last
    var east: String = ""
    var west: String = ""
    tile.foreach(el => {
      west = west :+ el.head
      east = east :+ el.last
    })

    val borders: (String, String, String, String) = (north, east, south, west)
    borders
  }

  def parseBorder(tiles: List[(Int, List[String])]): Map[Int, (String, String, String, String)] = {
    var tilesMap: Map[Int, (String, String, String, String)] = Map[Int, (String, String, String, String)]()
    tiles.foreach(tile => {
        tilesMap += (tile._1 -> getBordersNESW(tile._2))
    })
    tilesMap
  }

  def challenge1(tiles: List[(Int, List[String])]): Long = {
    val tilesMap = parseBorder(tiles)
    val borders = tilesMap.flatMap(tile => tile._2.productIterator.toList) ++ tilesMap.flatMap(tile => tile._2.productIterator.toList.map(str => str.toString.reverse))
    var prodBorders: Long = 1
    tilesMap.foreach( tile => {
      val isMaxNorth = if(borders.count(_==tile._2._1) == 1) 1 else 0
      val isMaxSouth = if(borders.count(_==tile._2._3) == 1) 1 else 0
      val isMaxEast = if(borders.count(_==tile._2._2) == 1) 1 else 0
      val isMaxWest = if(borders.count(_==tile._2._4) == 1) 1 else 0
      if((isMaxEast + isMaxNorth + isMaxSouth + isMaxWest) == 2) {
        println(tile._1)
        prodBorders = prodBorders * tile._1
      }
    })
    prodBorders
  }

  def main(args: Array[String]): Unit = {
    val list: List[String] = ReadFile.readFile("jour20.txt")
    println("Day 20 !")
    val tiles: List[(Int, List[String])] = parseInput(list)
    println(getBordersNESW(tiles.head._2))
    println("Challenge 1 : " + challenge1(tiles))
  }
}