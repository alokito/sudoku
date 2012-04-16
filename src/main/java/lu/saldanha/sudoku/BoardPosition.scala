package lu.saldanha.sudoku

class BoardPosition(val row:Int, val col:Int, val value:Int) {
	override def toString():String =
	  "("+row +"," +col+") = " +value 
}