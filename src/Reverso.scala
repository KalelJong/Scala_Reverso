object Reverso extends App {
  val boardSize = 8
  type Board = List[List[Option[Boolean]]]

  val initialBoard: Board = List.fill(boardSize, boardSize)(None)
    .updated(3, List.fill(boardSize)(None).updated(3, Some(false)).updated(4, Some(true)))
    .updated(4, List.fill(boardSize)(None).updated(3, Some(true)).updated(4, Some(false)))

  def clearConsole(): Unit = {
    print("\u001b[2J")
    print("\u001b[H")
  }

  def boardCLI(board: Board, validMoves: List[(Int, Int)] = List.empty): Unit = {
    clearConsole()
    println("  a b c d e f g h")
    println(" +----------------")
    board.zipWithIndex.foreach { case (row, i) =>
      print(s"${i + 1}|")
      row.zipWithIndex.foreach { case (cell, j) =>
        if (validMoves.contains((i, j))) {
          print(". ")
        } else {
          cell match {
            case Some(true)  => print("B ")
            case Some(false) => print("W ")
            case None        => print("  ")
          }
        }
      }
      println("|")
    }
    println(" +----------------")
  }

  def game(board: Board = initialBoard, currentPlayer: Boolean = true): Unit = {
    boardCLI(board)
    println("Game over!")
  }

  game()
}