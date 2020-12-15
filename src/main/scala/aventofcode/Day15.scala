package aventofcode

object Day15 {

  // For int given / last time this int was given
  var memory: Map[Int, Int] = Map[Int, Int]()

  def challenge1(numbersList: List[Int], xThNumberSpoken: Int): Int = {
    if(xThNumberSpoken == 0) return numbersList.last
    val lastNumber = numbersList.last
    val newNumber = {
      if (memory.contains(lastNumber)) {
        val difference = numbersList.size - memory(lastNumber)
        memory += (lastNumber -> numbersList.size)
        difference
      }
      else {
        memory += (lastNumber -> numbersList.size)
        0
      }
    }
    challenge1(numbersList :+ newNumber, xThNumberSpoken - 1)
  }

  def main(args: Array[String]): Unit = {
    val list: List[Int] = List(0, 12, 6, 13, 20, 1, 17)
    println("Day 15 !")

    for (it <- 0 until list.size - 1) memory += (list(it) -> (it + 1))
    println("Challenge 1 : " + challenge1(list, 2020 - list.size))

    memory = Map[Int, Int]()
    for (it <- 0 until list.size - 1) memory += (list(it) -> (it + 1))
    println("Challenge 2 : " + challenge1(list, 30000000 - 3))
  }
}
