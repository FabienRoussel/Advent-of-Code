package adventofcode

import scala.io.Source

object ReadFile {

    def readFile(string: String): List[String] = {
        val bufferedSource              = Source.fromResource(string)
        var content                     = List[String]()
        content = bufferedSource.getLines().toList
        bufferedSource.close()
        content
    }

}
