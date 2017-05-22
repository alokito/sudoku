package lu.saldanha.sudoku
import lu.saldanha.sudoku.parse.PositionLoader
import lu.saldanha.sudoku.board.BoardI
import lu.saldanha.sudoku.board.BoardPosition
import scala.util.control.Exception
import scala.Option;

object SudokuSolver {
  private val debug = 0
  def main(args: Array[String]) = {
     if (args.length == 0) {
       throw new Exception("Please provide valid board as first argument")
     }
     val startBoard = PositionLoader.loadBoard(args(0))
     println(startBoard.toString)
     if (!startBoard.isValid()) {
    		 println("Start board is NOT valid")
     } else {
       solve(startBoard.emptyPlaces, startBoard) match {
         case Some(endBoard) => println("SolveD!\n"+ endBoard.toString)
         case None => println("Failed to solve!")
       }
     }
  }
  def solve(places:List[Tuple2[Int,Int]], board:BoardI, guesses:Int = 0):Option[BoardI] = {
    val thisBoardMoves = board.movesForPosition _
    val possiblePlays = places.map(thisBoardMoves).filter(_.length > 0)
    val uniquePlays:List[BoardPosition] = possiblePlays.filter(_.length == 1).map(_.head)
    if (places.isEmpty) {
       Some(board)
    } else if (uniquePlays.isEmpty) {
        if (debug > 1) {
          println(board)
        }
        if (possiblePlays.isEmpty) {
          None
        } else {
          val byRegion = board.movesUniqueWithinRegions()
          if (byRegion.isEmpty) {
            val sorted = possiblePlays.sortBy(_.length)
            printSummary("multiple possible moves, must choose one("+guesses+" guesses so far)...: ",sorted)
            lookAheadSearch(sorted.flatten, places, board, guesses)
          } else {
            if (debug > 1) {
              printSummary("unique with region: ", byRegion)
            }
            makeMove(places, board, byRegion.head, guesses)
          }
        }
    } else {
      if (debug > 1) {
        println(board)
        printSummary("unique moves: ", uniquePlays)
      }
      makeMove(places, board, uniquePlays.head, guesses)
    }
  }

  private def lookAheadSearch(moves:List[BoardPosition], places:List[Tuple2[Int,Int]], board:BoardI, guesses:Int):Option[BoardI] = {
    val movesAfter = moves.map(move => {
      val newBoard = board.deriveBoard(move)
      val thisBoardMoves = newBoard.movesForPosition _
      val possiblePlays = places.filter(place => place._1 != move.row || place._2 != move.col).map(thisBoardMoves).flatten
      (possiblePlays.length, move)
    }).sortBy(_._1)
    searchList(movesAfter, places, board, guesses)
  }

  private def searchList(moves:List[Tuple2[Int,BoardPosition]], places:List[Tuple2[Int,Int]], board:BoardI, guesses:Int):Option[BoardI] = {
    if (moves.length == 0) {
      None
    } else {
      val bestMove = moves.head
      println("best move" + bestMove._2 +", remaining plays " + bestMove._1)
      makeMove(places, board, bestMove._2, guesses + 1) match {
        case Some(board) => Some(board)
        case None => searchList(moves.tail, places, board, guesses)
      }
    }
  }

  private def depthFirstSearch(moves:List[BoardPosition], places:List[Tuple2[Int,Int]], board:BoardI, guesses:Int):Option[BoardI] = {
    if (moves.isEmpty) {
      println("out of moves!")
      None
    } else {
      println(guesses + ") trying " + moves.head)
      makeMove(places, board, moves.head, guesses + 1) match {
        case Some(board) => Some(board)
        case None => depthFirstSearch(moves.tail, places, board, guesses)
      }
    }
  }
  private def printSummary(title:String, moves:Seq[Object]) = println(title + (moves.head.toString /: moves.tail) ( _ + ", " + _))
  private def makeMove(places:List[Tuple2[Int,Int]], board:BoardI, boardPosition: BoardPosition, guesses:Int) =
    solve(places.filterNot(x => x._1 == boardPosition.row && x._2 == boardPosition.col),
    board.deriveBoard(boardPosition), guesses)
}
