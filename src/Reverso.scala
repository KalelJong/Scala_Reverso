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

  def isMoveValid(board: Board, row: Int, col: Int, player: Boolean): Boolean = {
    if (board(row)(col).isDefined) false // If piece is on field => returns false
    else {
      val directions = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
      directions.exists { case (rowDirection, columnDirection) =>
        val positions = LazyList.unfold((row + rowDirection, col + columnDirection)) {
          case (r, c) if r >= 0 && r < board.size && c >= 0 && c < board.size =>
            Some(((r, c), (r + rowDirection, c + columnDirection)))
          case _ => None
        }
        val enemiesInDirection = positions.takeWhile { case (r, c) => board(r)(c).contains(!player) }.toList
        enemiesInDirection.nonEmpty && {
          val (r, c) = (row + rowDirection * (enemiesInDirection.size + 1), col + columnDirection * (enemiesInDirection.size + 1))
          r >= 0 && r < board.size && c >= 0 && c < board.size && board(r)(c).contains(player)
        }
      }
    }
  }

  def applyMove(board: Board, row: Int, col: Int, player: Boolean): Board = {
    val directions = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
    val updatedBoard = board.updated(row, board(row).updated(col, Some(player)))

    directions.foldLeft(updatedBoard) { case (updtBoard, (rowDirection, colDirection)) =>
      val enemiesToFlip = for {
        (r, c) <- LazyList.unfold((row + rowDirection, col + colDirection)) {
          case (r, c) if r >= 0 && r < board.size && c >= 0 && c < board.size && board(r)(c).contains(!player) =>
            Some(((r, c), (r + rowDirection, c + colDirection)))
          case _ => None
        }
      } yield (r, c)

      if (enemiesToFlip.nonEmpty) {
        val (fr, fc) = (row + rowDirection * (enemiesToFlip.size + 1), col + colDirection * (enemiesToFlip.size + 1))
        if (fr >= 0 && fr < board.size && fc >= 0 && fc < board.size && board(fr)(fc).contains(player)) {
          enemiesToFlip.foldLeft(updtBoard) { case (brd, (fr, fc)) =>
            brd.updated(fr, brd(fr).updated(fc, Some(player)))
          }
        } else updtBoard
      } else updtBoard
    }
  }

  def game(board: Board = initialBoard, currentPlayer: Boolean = true): Unit = {
    boardCLI(board)
    println("Game over!")
  }

  game()
}