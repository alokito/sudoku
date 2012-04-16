package lu.saldanha.sudoku

object TabDelimitedText {
	/**
	 * This assumes no embedded tabs (i.e. no tabs in the strings)
	 *  
	 * This function expects an iterator over lines in a file.
	 * If the last token begins with a " but does not end with one,
	 * it will be treated as a quoted string. In this case, it will
	 * call "parseToQuote" to get the rest of the line.
	 */
	def parseToRow(iter:Iterator[String]):List[String] = 
		if (iter.hasNext) 
		  parseToRow(nextRow(iter), iter) 
		else 
		  Nil
	
	private def parseToRow(row:List[String], iter:Iterator[String]):List[String] =
		if (row.last.startsWith("\"") && !row.last.endsWith("\""))
			glueLastFirst(row, parseToQuote(nextRow(iter), iter)) 
		else 
		  row

	private def parseToQuote(row:List[String], iter:Iterator[String]):List[String] =
		if (row.size > 1 || row.first.endsWith("\""))
			parseToRow(row, iter);
		else
			glueLastFirst(row, parseToQuote(nextRow(iter), iter)) 

	private def nextRow(iter:Iterator[String]):List[String] = 
		iter.next().split("\t").elements.toList
	
	private def glueLastFirst(row:List[String], rest:List[String]):List[String] = {
		val middlePart:String = row.last + " " + rest.first
		row.init :::  (middlePart :: rest.tail) 
	}
}