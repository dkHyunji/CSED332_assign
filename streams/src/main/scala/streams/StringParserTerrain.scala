package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 * 
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 * 
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 * 
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     inside the terrain)
 * 
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   * 
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   * 
   * is represented as
   * 
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = { position: Pos => validPosition(levelVector, position) }

  def validPosition(levelVector: Vector[Vector[Char]], position: Pos): Boolean = {

    def calculatingPosition(levelVector: Vector[Vector[Char]], position: Pos, x_axis: Int): Boolean = {
      if (levelVector.isEmpty) false
      else {
        if (position.x == x_axis) validColumn(levelVector.head, position.y)
        else calculatingPosition(levelVector.tail, position, x_axis + 1)
      }
    }
    calculatingPosition(levelVector, position, 0)
  }

  def validColumn(row: Vector[Char], y_position: Int): Boolean = {
    if (y_position >= row.size || y_position < 0) false
    else row(y_position) match {
      case ('o' | 'S' | 'T') => true
      case _ => false
    }

  }
  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    def obtainPosition(c: Char, levelVector: Vector[Vector[Char]], x_axis: Int): Pos = {
      if (levelVector.isEmpty) new Pos(-1, -1)
      else {
        val y_axis = levelVector.head.indexOf(c, 0)
        y_axis match {
          case x if x == -1 => obtainPosition(c, levelVector.tail, x_axis + 1)
          case _ => new Pos(x_axis, y_axis)
        }
      }
    }
    obtainPosition(c, levelVector, 0)
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
