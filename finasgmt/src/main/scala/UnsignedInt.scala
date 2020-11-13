object UnsignedInt {
  def unsignedInt(a:Long):scala.math.BigInt= {
    val prefix:Array[Byte]=Array(0)
    BigInt(prefix ++ BigInt(a).toByteArray)
  }
}