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

  def hasValidMoves(board: Board, player: Boolean): Boolean = {
    board.indices.exists(row => board(row).indices.exists(col => isMoveValid(board, row, col, player)))
  }

  def validMoves(board: Board, player: Boolean): List[(Int, Int)] = {
    (for {
      row <- board.indices
      col <- board(row).indices
      if isMoveValid(board, row, col, player)
    } yield (row, col)).toList
  }

  def game(board: Board = initialBoard, currentPlayer: Boolean = true): Unit = {
    if (hasValidMoves(board, player = true) || hasValidMoves(board, player = false)) {
      val validMovesList = validMoves(board, currentPlayer)
      boardCLI(board, validMovesList)

      if (validMovesList.nonEmpty) {
        println(s"Player ${if (currentPlayer) "B" else "W"}'s turn.")
        try {
          val input = scala.io.StdIn.readLine("Enter move (e.g., d3): ")
          if (input.length != 2) throw new IllegalArgumentException("Invalid input format. Please enter a letter followed by a number (e.g., d3).")

          val col = input.charAt(0) - 'a'
          val row = input.charAt(1).asDigit - 1

          if (col < 0 || col >= boardSize || row < 0 || row >= boardSize) throw new IndexOutOfBoundsException("Move is out of bounds. Please try again.")

          if (isMoveValid(board, row, col, currentPlayer)) {
            val newBoard = applyMove(board, row, col, currentPlayer)
            game(newBoard, !currentPlayer)
          } else {
            println("Invalid move, try again.")
            game(board, currentPlayer)
          }
        } catch {
          case e: IllegalArgumentException =>
            println(e.getMessage)
            game(board, currentPlayer)
          case e: IndexOutOfBoundsException =>
            println(e.getMessage)
            game(board, currentPlayer)
          case _: Throwable =>
            println("An unexpected error occurred. Please try again.")
            game(board, currentPlayer)
        }
      } else {
        println(s"No valid moves for player ${if (currentPlayer) "B" else "W"}. Skipping turn.")
        game(board, !currentPlayer)
      }
    } else {
      boardCLI(board)
      println("Game over!")
    }
  }

  game()
}