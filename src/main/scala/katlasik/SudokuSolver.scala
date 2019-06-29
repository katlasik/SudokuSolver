package katlasik

import scala.annotation.tailrec
import scala.util.control.TailCalls.{TailRec, done, tailcall}

object SudokuSolver {

  private lazy val All = "123456789".toSet

  type Board = Vector[Vector[Char]]
  type Coordinates = (Int, Int)

  def solve(unsolved: String): Option[String] = {

    val n = math.sqrt(unsolved.length).toInt

    val board = partition(unsolved.iterator, n)

    def traverse(
        chars: List[Char],
        board: Board,
        coordinates: Coordinates,
        next: Coordinates
    ): TailRec[List[Option[Board]]] = {
      chars match {
        case Nil => done(Nil)
        case x :: xs =>
          for {
            a <- iterate(updateBoard(board, coordinates, x), next)
            b <- traverse(xs, board, coordinates, next)
          } yield a :: b
      }
    }

    def iterate(board: Board, coordinates: Coordinates): TailRec[Option[Board]] = {

      val n = board.size

      (coordinates, maybeNextCoordinates(coordinates, n)) match {
        case ((x, y), Some(next)) =>
          if (board(y)(x) != '_') {
            tailcall(iterate(board, next))
          } else {
            traverse(availableCharacters(board, coordinates).toList, board, coordinates, next).map(_.flatten.headOption)
          }
        case _ => done(availableCharacters(board, coordinates).headOption.map(c => updateBoard(board, coordinates, c)))
      }
    }

    iterate(board, (0, 0)).result.map(_.map(_.mkString("")).mkString(""))
  }

  def availableCharacters(board: Board, coordinates: Coordinates): Set[Char] = {

    val charsInLines = coordinates match {
      case (x, y) =>
        (0 until board.size).toSet.flatMap((j: Int) => Set(board(j)(x), board(y)(j)))
    }

    lazy val squareSize = math.sqrt(All.size).toInt

    val charsInSquare = (coordinates match {
      case (x, y) => {
        val (xmod, ymod) = (x / squareSize, y / squareSize)

        for {
          i <- 0 until squareSize
          j <- 0 until squareSize
        } yield {
          board(j + (ymod * squareSize))(i + (xmod * squareSize))
        }
      }
    }).toSet

    All -- (charsInLines ++ charsInSquare)
  }

  def updateBoard(board: Board, coordinates: (Int, Int), c: Char): Board = {
    coordinates match {
      case (x, y) => board.updated(y, board(y).updated(x, c))
    }
  }

  def maybeNextCoordinates(coordinates: Coordinates, n: Int): Option[Coordinates] = coordinates match {
    case (x, y) if x == n - 1 => if (y == n - 1) None else Some((0, y + 1))
    case (x, y)               => Some(x + 1, y)
  }

  @tailrec
  def partition(l: Iterator[Char], n: Int, acc: Board = Vector.empty): Board = {
    l.splitAt(n) match {
      case (head, tail) => if (head.isEmpty) acc else partition(tail, n, acc.appended(head.toVector))
    }
  }

}
