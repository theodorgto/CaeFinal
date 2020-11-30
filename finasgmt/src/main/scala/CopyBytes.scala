import java.nio.file.{Files, Paths}

object CopyBytes {
  def apply(fileName: String): List[Int] =
    Files.readAllBytes(Paths.get(fileName)).grouped(4).map(
      _.reverse.foldLeft(0)((a, b) => a << 8 | (b & ((1 << 8) - 1)))
    ).toList
}