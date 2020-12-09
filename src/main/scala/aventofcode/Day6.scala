package aventofcode

import scala.io.Source

object Day6 {

    def readFile(): List[String] = {
        val bufferedSource              = Source.fromResource("jour6.txt")
        var content                     = List[String]()
        content = bufferedSource.getLines().toList
        bufferedSource.close()
        content
    }

    def countLettersinSubGroup(str: String): Int = {
        str.distinct.toCharArray.length
    }

    def countUniqueLettersInGroups(): Int = {
        val lettersGroupList: List[String]      = readFile() :+ ""
        var countLetters: Int                   = 0
        var lettersGroup: String                = ""
        lettersGroupList.foreach(el => {
            if(el != ""){
                lettersGroup = lettersGroup.concat(el)
            }
            else{
                countLetters += countLettersinSubGroup(lettersGroup)
                lettersGroup = ""
            }
        })
        countLetters
    }

    def countCommontLettersinSubGroup(strList: List[String]): Int = {
        var fullSet: Set[Char] = strList.head.toSet
        strList.foreach(el => {
            fullSet = fullSet.intersect(el.toSeq.distinct.unwrap.toSet)
        })
        fullSet.size
    }

    def countCommonLettersInGroups(): Int = {
        val lettersGroupList: List[String]      = readFile() :+ ""
        var countLetters: Int                   = 0
        var lettersGroup: List[String]          = List[String]()
        lettersGroupList.foreach(el => {
            if(el != ""){
                lettersGroup = lettersGroup :+ el
            }
            else{
                countLetters += countCommontLettersinSubGroup(lettersGroup)
                lettersGroup = List[String]()
            }
        })
        countLetters
    }

    def main(args: Array[String]): Unit = {
        println("Day 6 !")
        //println(countUniqueLettersInGroups())
        println(countCommonLettersInGroups())
    }
}