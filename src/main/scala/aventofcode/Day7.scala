package aventofcode

import scala.collection.mutable.Queue
import scala.io.Source

object Day7 {

    def readFile(): List[String] = {
        val bufferedSource              = Source.fromResource("jour7.txt")
        var content                     = List[String]()
        content = bufferedSource.getLines().toList
        bufferedSource.close()
        content
    }

    // PART I
    def extractContentfromContainer(str: String): (String, List[String]) = {
        val array                       = str.dropRight(1).split("s contain ")
        val container: String           = array.head
        val contentsArray: Array[String]= array(1).split(", ")
        var contents: List[String]      = List()
        var amount: Int                 = 0
        var source: String              = ""

        if(contentsArray.head != "no other bags"){
            contentsArray.foreach(el => {
                amount = el.head.toString.toInt
                source = if(amount == 1) el.slice(2, el.length) else el.slice(2, el.length-1)
                contents = contents :+ source
            })
        }

        (container, contents)
    }

    def countBagOfColorsContainingSGBag(): Int = {
        val rulesList: List[String]     = readFile()
        var graph: Map[String, Array[String]] = Map[String, Array[String]]()

        rulesList.foreach(el => {
            val (container, contents) = extractContentfromContainer(el)
            if(!graph.contains(container)){
                graph += (container -> Array[String]())
            }
            contents.foreach(content => {
                if(!graph.contains(content)){
                    graph += (content -> Array[String]())
                }
                graph = graph.updated(content, graph.getOrElse(content, Array.empty[String]) :+ container)
            })
        })

        var nodeQueue: Queue[String]  = Queue("shiny gold bag")
        var visitedNodes: Set[String] = Set()
        while(nodeQueue.nonEmpty){
            val str: String = nodeQueue.dequeue()
            if(!visitedNodes.contains(str)) {
                graph(str).foreach(el =>{
                    nodeQueue += el.toString
                })
                visitedNodes += str
            }
        }

        visitedNodes.size - 1
    }

    // PART II
    def extractContentfromContainerWithAmount(str: String): (String, Map[String, Int]) = {
        val array                       = str.dropRight(1).split("s contain ")
        val container: String           = array.head
        val contentsArray: Array[String]= array(1).split(", ")
        var contents: Map[String, Int]  = Map[String, Int]()
        var amount: Int                 = 0
        var source: String              = ""

        if(contentsArray.head != "no other bags"){
            contentsArray.foreach(el => {
                amount = el.head.toString.toInt
                source = if(amount == 1) el.slice(2, el.length) else el.slice(2, el.length-1)
                contents += (source -> amount)
            })
        }

        (container, contents)
    }


    def goThroughGraph(graph: Map[String, Map[String, Int]], str: String): Int = {
        var count = 1;
        graph(str).foreach(el => {
            val amount = el._2
            count += amount * goThroughGraph(graph, el._1)
        })
        count
    }

    def countNumberOfBagOfColorsInSGBag(): Int = {
        val rulesList: List[String]                 = readFile()
        var graph: Map[String, Map[String, Int]]    = Map[String, Map[String, Int]]()

        rulesList.foreach(el => {
            val (container, contents) = extractContentfromContainerWithAmount(el)
            graph += (container -> contents)
        })
        //graph.foreach{case (key, value) => println (key + "-->" + value)}

        goThroughGraph(graph, "shiny gold bag") - 1
    }

    def main(args: Array[String]): Unit = {
        println("Day 7 !")
        println(countBagOfColorsContainingSGBag())
        println(countNumberOfBagOfColorsInSGBag())
    }
}
