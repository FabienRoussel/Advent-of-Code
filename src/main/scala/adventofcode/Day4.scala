package adventofcode

import scala.io.Source

object Day4 {

    def readAndFormatFile(): List[String] = {
        val bufferedSource              = Source.fromResource("jour4.txt")
        var file                        = List[String]()
        var passportLine: String        = ""

        // TODO: refacto pour ne pas avoir de if après la boucle
        for (fileLine <- bufferedSource.getLines) {
            if(fileLine == ""){
                file ::= passportLine.trim()
                passportLine = ""
            }
            else {
                passportLine = passportLine + " " + fileLine
            }
        }
        if(passportLine != ""){
            file ::= passportLine.trim()
            passportLine = ""
        }
        bufferedSource.close

        file.reverse
    }

    def computeNbValidPassportChallenge1(): Int = {
        val passportsList: List[String]         = readAndFormatFile()

        var countValidPassport: Int             = 0
        var passportKeysValues                  = Array[String]()
        var key: String                         = ""

        // cid (Country ID)
        val validPassport : Set[String]         = Set("byr","iyr","eyr","hgt","hcl","ecl","pid")
        var analyzePassport : Set[String]       = Set()
        for(passport <- passportsList){
            passportKeysValues = passport.split(" ")
            for(keyValue <- passportKeysValues){
                key = keyValue.split(":")(0)
                analyzePassport += key
            }
            if(validPassport.subsetOf(analyzePassport)){
                countValidPassport += 1
            }

            analyzePassport = Set()
        }
        countValidPassport
    }

    def isValidHGT(value: String): Boolean = {
        if(value.length > 2){
            (value.matches("[0-9]+cm") && value.dropRight(2).toInt >= 150  && value.dropRight(2).toInt <= 193
              || value.matches("[0-9]+in") && value.dropRight(2).toInt >= 59  && value.dropRight(2).toInt <= 76)
        }
        else false
    }

    def isValidHCL(value: String): Boolean = {
        if(value.length == 7){
            value.matches("#[a-fA-F0-9]+")
        }
        else false
    }

    def isValidData(key: String, value: String): Boolean = key match {
        case "byr" => value.toInt >= 1920 && value.toInt <= 2002
        case "iyr" => value.toInt >= 2010 && value.toInt <= 2020
        case "eyr" => value.toInt >= 2020 && value.toInt <= 2030
        case "hgt" => isValidHGT(value)
        case "hcl" => isValidHCL(value)
        case "ecl" => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
        case "pid" => value.length == 9 && value.matches("[0-9]+")
        case "cid" => true
        case _ => false
    }

    def computeNbValidPassportChallenge2(): Int = {
        val passportsList: List[String]         = readAndFormatFile()

        var countValidPassport: Int             = 0
        var passportKeysValues                  = Array[String]()
        var key: String                         = ""
        var value: String                       = ""

        // cid (Country ID)
        val validPassport : Set[String]         = Set("byr","iyr","eyr","hgt","hcl","ecl","pid")
        var analyzePassport : Set[String]       = Set()
        for(passport <- passportsList){
            passportKeysValues = passport.split(" ")

            for(keyValue <- passportKeysValues){
                key = keyValue.split(":")(0)
                value = keyValue.split(":")(1)
                if(isValidData(key, value))
                    analyzePassport += key
            }

            if(validPassport.subsetOf(analyzePassport)){
                countValidPassport += 1
            }

            analyzePassport = Set()
        }
        countValidPassport
    }

    def main(args: Array[String]): Unit = {
        println("Day 4 !")
        println("Challenge 1 : " + computeNbValidPassportChallenge1())
        println("Challenge 2 : " + computeNbValidPassportChallenge2())
    }

}
