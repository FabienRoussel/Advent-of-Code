package aventofcode

object Day15 {

  // For int given / last time this int was given
  var memory: Map[Int, Int] = Map[Int, Int]()

  def challenge1(lastNumber: Int, xThNumberSpoken: Int, turn: Int): Int = {
    if(xThNumberSpoken == 0) return lastNumber
    val newNumber = {
      if (memory.contains(lastNumber)) {
        val difference = turn - memory(lastNumber)
        memory += (lastNumber -> turn)
        difference
      }
      else {
        memory += (lastNumber -> turn)
        0
      }
    }
    challenge1(newNumber, xThNumberSpoken - 1, turn + 1)
  }

  def main(args: Array[String]): Unit = {
    val list: List[Int] = List(0, 12, 6, 13, 20, 1, 17)
    println("Day 15 !")

    for (it <- 0 until list.size - 1) memory += (list(it) -> (it + 1))
    println("Challenge 1 : " + challenge1(list.last , 2020 - list.size, list.size))

    memory = Map[Int, Int]()
    for (it <- 0 until list.size - 1) memory += (list(it) -> (it + 1))
    println("Challenge 2 : " + challenge1(list.last, 30000000 - list.size, list.size))
  }
}
