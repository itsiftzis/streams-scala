package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooR
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(5,8)), "5,8")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      lazy val tPos: Pos = findChar('T', Vector(level.split("\n").map(str => Vector(str: _*)): _*))
      assert(tPos == Pos(4,7))
      lazy val rPos: Pos = findChar('R', Vector(level.split("\n").map(str => Vector(str: _*)): _*))
      assert(rPos == Pos(3,9))
    }
  }

  test("history level 1"){
    new Level1 {
      val test = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      assert(test.head == (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)) )
      assert(test.tail.head == (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)) )
    }
  }

  test("new neighbors level 1") {
    new Level1 {
      val test = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)),
          (Block(Pos(3,1),Pos(3,3)), List(Down,Down,Right))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )
      assert(test == Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)),
        (Block(Pos(3,1),Pos(3,3)), List(Down,Down,Right))
      ).toStream)
    }

    new Level1 {
      val test = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )
      assert(test == Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream)
    }
  }

  /*test("from function") {
    new Level1 {
      val test = from(Stream.cons((startBlock, List[Move]()), Stream.empty), Set[Block]())
      println(test.toList.take(3))
    }
  }*/

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
