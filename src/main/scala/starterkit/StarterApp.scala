package starterkit

object StarterApp {

  def main(args: Array[String]): Unit = {
    val env = ApplicationConfig.ENV
    val starterConfig = ApplicationConfig.STARTER_CONFIG

    val outputSentence = buildOutputSentence(env, starterConfig)
    println(outputSentence)
  }

  def buildOutputSentence(env: String, starterConfig: StarterConfig): String = {
    val name = starterConfig.starterName
    val repeater = starterConfig.starterRepeat

    s"""
      | Hi !
      | Welcome on ScalaStarterKit :D
      |
      | You're on environment ${env}.
      |
      | You're name will be printed ${repeater} times.
      | ${name.repeat(repeater)}
      |
      | Have fun with scala !!
      |""".stripMargin
  }

}
