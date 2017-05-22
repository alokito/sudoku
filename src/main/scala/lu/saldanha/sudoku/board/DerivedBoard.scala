package lu.saldanha.sudoku.board

class DerivedBoard(val play:BoardPosition, val underlying:BoardI) extends BoardI {
  def getCell(row:Int, col:Int):Option[Int] =
	  if (play.row == row && play.col == col) 
		Some(play.value) 
	  else 
	    underlying.getCell(row, col)
  def isValid():Boolean = {
    (countInColOf(play.col, play.value) == 1) && 
    (countInRowOf(play.row, play.value) == 1) && 
    (countInQuadOf(getQuad(play.row,play.col), play.value) == 1) && 
    underlying.isValid()
  }
}