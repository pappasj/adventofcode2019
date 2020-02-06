object Day1 {

  val input: String = scala.io.Source.fromResource("day1.txt").mkString.trim

  def part1RequiredFuel(mass: Int): Int = mass / 3 - 2

  def part2RequiredFuel(mass: Int): Int = {
    val fuel = part1RequiredFuel(mass)
    if (fuel <= 0)
      0
    else
      fuel + part2RequiredFuel(fuel)
  }

  def parseMasses(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  def main(args: Array[String]): Unit = {
      println(parseMasses(input).map(part1RequiredFuel).sum)
      println(parseMasses(input).map(part2RequiredFuel).sum)
  }
}
