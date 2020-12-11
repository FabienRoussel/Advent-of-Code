package aventofcode

object Day10 {

    def challenge10(): Unit = {
        val adapterList: List[String]       = ReadFile.readFile("jour10.txt")
        adapterList.foreach(el => println(el))
    }

    def main(args: Array[String]): Unit = {
        println("Day 10 !")
        println(challenge10())
    }
}
