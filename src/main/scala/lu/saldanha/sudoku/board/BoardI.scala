package lu.saldanha.sudoku.board

object BoardI {
  def getEmptyBoard() = EmptyBoard
  def getBoardPosition(row:Int, col:Int, value:Int) = new BoardPosition(row, col, value)
}

abstract class BoardI {
  // these two are abstract
  def getCell(t:Tuple2[Int, Int]):Option[Int] = getCell(t._1,t._2)
  def getCell(row:Int, col:Int):Option[Int]
  def isValid():Boolean
  
  def getSize():Int = 9
  def emptyPlaces:List[Tuple2[Int,Int]] =
    allPlaces.filter(x => getCell(x) match {
      case Some(y) => false
      case None => true
    })
  
  override def toString():String = {
    val size = getSize()
    val rows = makeInts(size).reverse
    val rowSummary = rows.map(x => summarizeRow(x) + "\n")
    var separator = makeSeparator(size)
    (rowSummary.head /: rowSummary.tail) (_  + _ )
  }

  /**
    * Moves that would result in a valid board for the specified position
    *
    * @param place
    * @return
    */
  def movesForPosition(place:Tuple2[Int,Int]):List[BoardPosition] = {
    val moves = makeInts(getSize()).map(new BoardPosition(place._1, place._2,_));
    val possible = moves.filter(deriveBoard(_).isValid())
    possible
  }

  def movesUniqueWithinRegions():List[BoardPosition] = {
    val ints = makeInts(getSize())
    val moves:List[BoardPosition] = ints.flatMap(i => allRegions
      .filter(!_.containsValue(i))
      .map(_.locationsForInt(i))
      .find(_.size == 1) match {
        case Some(boardPosition) => boardPosition
        case None => Nil
      })
    moves
  }



  def deriveBoard(position:BoardPosition):BoardI =
    getCell(position.row,position.col) match {
      case Some(x) => throw new Exception("Error: Cannot set position " +
          position.toString()+" becuase it already has value " + x)
      case None => 
        new DerivedBoard(position, this)
    }



  private def allRegions:List[BoardRegion]
    = makeInts(getSize()).flatMap(i => List(allPlacesInQuad(i), allPlacesInRow(i), allPlacesInCol(i))).map(new BoardRegion(_,this))

  private def allPlacesInQuad(quad:Int):List[Tuple2[Int,Int]] = allPlaces.filter(p => getQuad(p._1, p._2) == quad)
  private def allPlacesInRow(row:Int):List[Tuple2[Int,Int]] = allPlaces.filter(p => p._1 == row)
  private def allPlacesInCol(col:Int):List[Tuple2[Int,Int]] = allPlaces.filter(p => p._1 == col)

  private def makeInts(x:Int):List[Int] =
    if (x == 0)
      Nil
    else
        x :: makeInts(x - 1)
  private val allPlaces:List[Tuple2[Int,Int]] = {
      val ints = makeInts(getSize())
      ints.flatMap( x => ints.map(y => new Tuple2(x,y)))
    }

  private def summarizeCell(row:Int, col:Int):String =
    getCell(row, col) match {
    	case Some(cell) => ""+cell
    	case None => " "
  	}
  private def summarizeRow(row:Int):String = {
      val cols = makeInts(getSize()).reverse
    val cellSummary = cols.map(x => summarizeCell(row, x))
    (cellSummary.head /: cellSummary.tail) (_ + "|" + _)
  }
  private def makeSeparator(size:Int):String = {
    val cols = makeInts(getSize()).reverse
    val sep = "-"
    (sep /: cols.tail) ((x,y) => x + "|" + sep)
  }
  
  // validation code
  def countInRowOf(row:Int, x:Int) =
    makeInts(getSize).filter(getCell(row, _) match {
      case Some(y) => y == x
      case None => false
    }).size
  def countInColOf(col:Int, x:Int) =
    makeInts(getSize).filter(getCell(_,col) match {
      case Some(y) => y == x
      case None => false
    }).size
  def countInQuadOf(quad:Int, value:Int) = {
    val rowOff:Int = 3* ((quad -1) / 3);
    val colOff:Int = 3 * ((quad -1) % 3);
    makeInts(getSize).filter(x => getCell(rowOff+((x-1)/3+1),colOff + (x-1)%3+1) match {
      case Some(y) => y == value
      case None => false
    }).size
  }

  def getQuad(row:Int, col:Int) =
    (col-1) /3 + 3 * ((row -1 )/3) + 1
  
}