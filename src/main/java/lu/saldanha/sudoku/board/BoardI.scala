package lu.saldanha.sudoku.board

object BoardI {
  def getEmptyBoard() = EmptyBoard
  def getBoardPosition(row:Int, col:Int, value:Int) = new BoardPosition(row, col, value)
}

abstract class BoardI {
  // these two are abstract
  def getCell(row:Int, col:Int):Option[Int]
  def isValid():Boolean
  
  def getSize():Int = 9
  def emptyPlaces:List[Tuple2[Int,Int]] =
    allPlaces.filter(x => getCell(x._1, x._2) match {
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
  def possiblePlays(place:Tuple2[Int,Int]):List[BoardPosition] = {
    val plays = makeInts(getSize()).map(new BoardPosition(place._1, place._2,_));
    val possible = plays.filter(deriveBoard(_).isValid())
    possible
  }
  
  def deriveBoard(position:BoardPosition) =
    getCell(position.row,position.col) match {
      case Some(x) => throw new Exception("Error: Cannot set position " +
          position.toString()+" becuase it already has value " + x)
      case None => 
        new DerivedBoard(position, this)
    }
  private def makeInts(x:Int):List[Int] =
    if (x == 0)
      Nil
    else
        x :: makeInts(x - 1)
  private def allPlaces:List[Tuple2[Int,Int]] = {
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