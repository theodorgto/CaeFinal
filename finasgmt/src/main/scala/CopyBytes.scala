import java.nio.file.{Files, Paths}

object CopyBytes {
  def apply(fileName: String): List[Long] = Files.readAllBytes(Paths.get(fileName))
    .grouped(4).map(_.reverse.foldLeft(0L)((a, b) => a << 8 | b.toLong)).toList
}

/*
object Main {
  def main(args: Array[String]): Unit = {
    println(CopyBytes("tests/task1/addpos.bin") map (_.toHexString))
  }
}
*/