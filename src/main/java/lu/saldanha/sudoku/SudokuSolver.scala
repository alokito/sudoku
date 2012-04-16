package lu.saldanha.sudoku

object SudokuSolver {
  def main(args: Array[String]) = {
     val startBoard = PositionLoader.loadBoard(args(0))
     println(startBoard.toString)
     if (!startBoard.isValid()) {
    		 println("Start board is NOT valid")
     } else {
       val endBoard = solve(startBoard.emptyPlaces, startBoard)
       println("SolveD!")
       println(endBoard.toString)
     }
  }
  def solve(places:List[Tuple2[Int,Int]], board:BoardI):BoardI = {
    val possiblePlays = places.map(board.possiblePlays(_));
    val uniquePlays:List[BoardPosition] = possiblePlays.filter(_.length == 1).map(_(0))
    if (uniquePlays.length == 0)
      if (places.length > 0)
        // could try and guess w/ backtracking on a "possible play"
        throw new Exception("Could not find unique position!")
      else
    	board
    else
      solve(places.filterNot(x => x._1 == uniquePlays(0).row && x._2 == uniquePlays(0).col),
          board.deriveBoard(uniquePlays(0)))
  }
}