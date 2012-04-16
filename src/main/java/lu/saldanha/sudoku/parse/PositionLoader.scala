package lu.saldanha.sudoku.parse
import lu.saldanha.sudoku.board.BoardI

object PositionLoader {
  // expects list of positions
  def loadBoard(file:String):BoardI = {
    applyPositions(BoardI.getEmptyBoard, scala.io.Source.fromFile(file).getLines())
  }
  private def applyPositions(soFar:BoardI, iter:Iterator[String]):BoardI = {
    if (!iter.hasNext)
      soFar
      else
        applyPositions(applyPosition(soFar, TabDelimitedText.parseToRow(iter)), iter)
  }
  
  private def applyPosition(soFar:BoardI, row:List[String]) = 
    soFar.deriveBoard(BoardI.getBoardPosition(row(0).toInt, row(1).toInt, row(2).toInt))

}