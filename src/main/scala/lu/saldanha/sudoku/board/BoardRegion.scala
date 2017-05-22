package lu.saldanha.sudoku.board

/**
  * Created by saldaal1 on 5/21/17.
  */
class BoardRegion(val places:List[Tuple2[Int,Int]], val board:BoardI) {
  def containsValue(value:Int):Boolean = places.exists(board.getCell(_) match {
    case Some(x) => x == value
    case None => false
  })

  /**
    * Moves for i within the region that would result in a valid board
    *
    * @param i
    */
  def locationsForInt(i:Int):List[BoardPosition] = {
    val moves = places.filter(board.getCell(_).isEmpty).map(t => new BoardPosition(t._1, t._2, i))
    val possible = moves.filter(board.deriveBoard(_).isValid())
    possible
  }
}
