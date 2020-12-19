package adventofcode


object Day10 {

  def joltAdapaterChallenge1(): Int = {
    val adapterListOrdered: List[Int] = ReadFile.readFile("jour10.txt").map(_.toInt).sorted

    val chainOfAdapterList = 0 +: adapterListOrdered :+ (adapterListOrdered.max+3)
    val adaptedDiffList = chainOfAdapterList.sliding(2, 1).toList.map(el => el(1) - el.head)

    val nbOf1 = adaptedDiffList.count(_ == 1)
    val nbOf3 = adaptedDiffList.count(_ == 3)

    println(nbOf1, nbOf3)

    nbOf1 * nbOf3
  }

  // As we cannot give reference in scala method...
  var map: Map[String, Long] = Map[String, Long]()
  def combination(adapterList: List[Int]): Long = {
    val hashKey = adapterList.mkString(",")
    if(map.contains(hashKey)){
      return map(hashKey)
    }

    var nbCombinaison: Long = 1
    for(it <- 1 until adapterList.size - 1){
      if((adapterList(it+1) - adapterList(it-1)) <= 3){
        val adapterListShortened: List[Int] = List(adapterList(it - 1)).concat(adapterList.slice(it + 1, adapterList.size))
        nbCombinaison += combination(adapterListShortened)
      }
    }
    map += (hashKey -> nbCombinaison)
    nbCombinaison
  }

  def joltAdapaterChallenge2(): Long = {
    val adapterListOrdered: List[Int] = ReadFile.readFile("jour10.txt").map(_.toInt).sorted
    val chainOfAdapterList = 0 +: adapterListOrdered :+ (adapterListOrdered.max+3)
    combination(chainOfAdapterList)
  }

  def main(args: Array[String]): Unit = {
    println("Day 10 !")
    println("Challenge 1 : " + joltAdapaterChallenge1())
    println("Challenge 2 : " + joltAdapaterChallenge2())
  }
}
