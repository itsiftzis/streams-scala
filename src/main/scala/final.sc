package streams
import streams.Bloxorz._
import streams.GameDef

object testakas {

val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
private lazy val vector: Vector[Vector[Char]] =
  Vector(level.split("\n").map(str => Vector(str: _*)): _*)

}