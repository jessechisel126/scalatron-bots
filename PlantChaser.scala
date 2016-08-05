// this is the source code for your bot - have fun!

import util.Random

class Bot {

  def respond(input: String) = {

    // Parse the command.
    val (opCode, paramMap) = CommandParser(input)

    // If we are reacting...
    if (opCode == "React") {

      // Get our view.
      val view = View(paramMap("view"))

      // Find the nearest food...
      view.offsetToNearest('P') match {

        // If food found, move toward it
        case Some(offset) =>
          val direction = offset.signum
          "Move(direction=" + direction + ")"

        // If no food found, do nothing
        case None =>
          val direction = XY.randomDirection(new Random())
          "Move(direction=" + direction + ")"
      }
    } else ""
  }
}

class ControlFunctionFactory {
  def create = new Bot().respond _
}

object CommandParser {

  def apply(command: String) = {

    def splitParam(param: String) = {
      val segments = param.split('=')
      (segments(0), segments(1))
    }

    // Command Format: "OpCode(param1=val1,param2=val2,param3=val3)"

    val segments = command.split('(')

    val (opCode, params) = (
      segments(0),              // "OpCode"
      segments(1).dropRight(1)  // "param1=val1,param2=val2,param3=val3")
      )

    val paramMap = params       // "param1=val1,param2=val2,param3=val3"
      .split(',')             // [ "param1=val1", "param2=val2", "param3=val3" ]
      .map(splitParam)        // [ ["param1","val1"] , ["param2","val2"] , ["param3","val3"] ]
      .toMap                  // { "param1"->"val1" , "param2"->"val2" , "param3"->"val3" }

    (opCode, paramMap)
  }
}

case class XY(xPos: Int, yPos: Int) {

  val (x, y) = (xPos, yPos)

  // Basic boolean checks
  def isZero        = x == 0 && y == 0
  def isNonZero     = !isZero
  def isPositive    = x > 0 && y > 0
  def isNonPositive = !isPositive
  def isNegative    = x < 0 && y < 0
  def isNonNegative = !isNegative

  // Basic vector math
  def +(other: XY): XY    = XY(x+other.x, y+other.y)
  def -(other: XY): XY    = XY(x-other.x, y-other.y)
  def *(scalar: Int): XY  = XY( x*scalar,  y*scalar)

  // Updates to position
  def updateX(newX: Int)  = XY(newX, y)
  def updateY(newY: Int)  = XY(x, newY)
  def addToX(dx: Int)     = XY(x+dx, y)
  def addToY(dy: Int)     = XY(x, y+dy)

  // Basic spacial properties
  def signum = XY(x.signum, y.signum)
  def length: Double = math.sqrt(x*x + y*y)
  def distanceTo(other: XY): Double = (this-other).length

  // Negation
  def negate  = XY(-x, -y)
  def negateX = XY(-x,  y)
  def negateY = XY( x, -y)

  override def toString: String = x + ":" + y
}

object XY {

  // Basic points
  def Zero      = XY(0, 0)
  def One       = XY(1, 1)

  // Up directions
  def UpLeft    = XY(-1, -1)
  def Up        = XY( 0, -1)
  def UpRight   = XY( 1, -1)

  // Right directions
  def RightUp   = XY( 1, -1)
  def Right     = XY( 1,  0)
  def RightDown = XY( 1,  1)

  // Down directions
  def DownRight = XY(-1,  1)
  def Down      = XY( 0,  1)
  def DownLeft  = XY( 1,  1)

  // Left directions
  def LeftDown  = XY(-1, -1)
  def Left      = XY(-1,  0)
  def LeftUp    = XY(-1,  1)

  // Random direction
  def randomDirection(rnd: Random) = XY(rnd.nextInt(3)-1, rnd.nextInt(3)-1)

  def fromString(s: String) = {
    val xyArr = s.split(':').map(_.toInt)
    XY(xyArr(0), xyArr(1))
  }
}

case class View(cells: String) {

  def apply(index: Int) = cells.charAt(index)

  // Square-side size of the view
  val size = math.sqrt(cells.length).toInt
  val center = XY(size/2, size/2)

  // Absolute conversions
  def indexFromAbsPos(absPos: XY)   = absPos.x + absPos.y * size
  def absPosFromRelPos(relPos: XY)  = relPos + center
  def absPosFromIndex(index: Int)   = XY(index % size, index / size)
  def cellAtAbsPos(pos: XY)         = cells(indexFromAbsPos(pos))

  // Relative conversions
  def indexFromRelPos(relPos: XY)   = indexFromAbsPos(relPos + center)
  def relPosFromAbsPos(absPos: XY)  = absPos - center
  def relPosFromIndex(index: Int)   = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(pos: XY)         = cells(indexFromRelPos(pos))

  def offsetToNearest(c: Char): Option[XY] = {

    // Get distances to each cell with value c
    val relativePositions =
    cells                             // "WP__M___P"
      .view                             // parse lazily
      .zipWithIndex                     // [ ('W',0), ('_',1), ('P',2), ... ]
      .filter(_._1 == c)                // [ ('P',2), ('P',9) ]
      .map(p => relPosFromIndex(p._2))  // [ 1, 1.4 ]

    if (relativePositions.isEmpty)
      None
    else
      Some(relativePositions.minBy(_.length))
  }
}