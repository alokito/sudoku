package lu.saldanha.sudoku.board

object EmptyBoard extends BoardI {
  def getCell(row: Int, col: Int): Option[Int] =  None 
  def isValid():Boolean = true
}