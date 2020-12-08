package starterkit

class StarterAppSpec extends AbstractSpec {
  "My test" when {
    "data are provided" should {
      "build the right sentence" in {
        // Given
        val env = "test"
        val starterConfig = StarterConfig("TestLucio", 2)

        // When
        val actual = StarterApp.buildOutputSentence(env, starterConfig)

        // Then
        val expected =
          """
            | Hi !
            | Welcome on ScalaStarterKit :D
            |
            | You're on environment test.
            |
            | You're name will be printed 2 times.
            | TestLucioTestLucio
            |
            | Have fun with scala !!
            |""".stripMargin
        actual shouldBe expected
      }
    }
  }
}
