case class Pos(x: Int, y: Int) {
  def dx(d: Int) = copy(x = x + d)
  def dy(d: Int) = copy(y = y + d)
}
type Terrain = Pos => Boolean

def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
  (pos: Pos) => (for (x <- 0 until levelVector.length-1;
                      y <- 0 until levelVector.toList(x).length-1
                      if (!levelVector.apply(x).apply(y).equals('-')
                        && pos.x == x && pos.y == y
                        )
  ) yield x).nonEmpty
}
val level =
  """ooo-------
    |oSoooo----
    |ooooooooo-
    |-ooooooooo
    |-----ooToo
    |------ooo-""".stripMargin
private lazy val vector: Vector[Vector[Char]] =
  Vector(level.split("\n").map(str => Vector(str: _*)): _*)
lazy val terrain: Terrain = terrainFunction(vector)
  terrain(Pos(0,0))
  terrain(Pos(4,11))
  terrain(Pos(5,9))
