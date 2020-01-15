object Day1 {

  trait Part {
    def requiredfuel(mass: Int): Int

    def totalRequiredFuel(masses: Seq[Int]): Int = masses.map(requiredfuel).sum
  }

  object Part1 extends Part {
    override def requiredfuel(mass: Int): Int = mass / 3 - 2
  }

  object Part2 extends Part {
    override def requiredfuel(mass: Int): Int = {
      val fuel = Part1.requiredfuel(mass)
      if (fuel <= 0)
        0
      else
        fuel + requiredfuel(fuel)
    }

  }

  def parseMasses(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  val input: String = scala.io.Source.fromResource("day1.txt").mkString.trim

  def main(args: Array[String]): Unit = {
      println(Part1.totalRequiredFuel(parseMasses(input)))
      println(Part2.totalRequiredFuel(parseMasses(input)))
  }
}
