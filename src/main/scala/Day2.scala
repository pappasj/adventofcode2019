object Day2 {

  val program: IndexedSeq[Int] = scala.io.Source.fromResource("day2.txt").mkString.trim.split(',').map(_.toInt)

  def executeWithReplacements(a: Int, b: Int): Int = {
    def exec(program: IndexedSeq[Int], pointer: Int = 0): Int = {
      def instruction(i: Int): Int = program(pointer + i)
      def opcode: Int = instruction(0)

      // parameter(0) is noun, parameter(1) is verb
      def parameter(i: Int): Int = instruction(i + 1)
      val next = pointer + 4

      opcode match {
        case 1 =>
          val overwriteValue = program(parameter(0)) + program(parameter(1))
          val newProgram = program.updated(parameter(2), overwriteValue)
          exec(newProgram, next)
        case 2 =>
          val overwriteValue = program(parameter(0)) * program(parameter(1))
          val newProgram = program.updated(parameter(2), overwriteValue)
          exec(newProgram, next)
        case 99 => program.head
        case _ => throw new IllegalArgumentException("Invalid opcode");
      }
    }
    exec(program.updated(1, a).updated(2, b))
  }

  case class NounVerbOutput(noun: Int, verb: Int, output: Int)
  def findNounAndVerb(program: IndexedSeq[Int], desiredOutput: Int): Option[(Int, Int)] = {
    val results = for {
      noun <- range
      verb <- range
    } yield NounVerbOutput(noun, verb, executeWithReplacements(noun, verb))

      results.find(_.output == desiredOutput).map(r => (r.noun, r.verb))
    }

  def range = 0 to 99

  def solveLastProblem(noun: Int, verb: Int): Int = {
    100 * noun + verb
  }

  // Part 2: Need to determine values for a and b such that the program returns output 19690720
  // 'a' is a noun, 'b' is the verb. Will be between 0 and 99
  // What is 100 * noun + verb?
  def main(args: Array[String]): Unit = {
    println(executeWithReplacements(12, 2));

    val desiredOutput = 19690720
    findNounAndVerb(program, desiredOutput).map(tup => solveLastProblem(tup._1, tup._2)).foreach(println(_))
  }

  // Improvements: Generalize addition and multiplication opcodes.
  // Stop running executeWithReplacements once result is found
}
