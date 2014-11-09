import streams.Bloxorz
import streams.GameDef
import streams.StringParserTerrain
case class Pos(x: Int, y: Int) {
  /** The position obtained by changing the `x` coordinate by `d` */
  def dx(d: Int) = copy(x = x + d)
  /** The position obtained by changing the `y` coordinate by `d` */
  def dy(d: Int) = copy(y = y + d)
}
type Terrain = Pos => Boolean
val level =
  """ooo-------
    |oSoooo----
    |ooooooooo-
    |-ooooooooo
    |-----ooToo
    |------ooo-""".stripMargin
private lazy val vector: Vector[Vector[Char]] =
  Vector(level.split("\n").map(str => Vector(str: _*)): _*)

def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
  val x = levelVector.indexWhere( (innerVector: Vector[Char]) => innerVector.indexOf(c) > -1 )
  val y = levelVector.toList(x).indexOf(c)
  Pos(x,y)
}
findChar('T', vector)