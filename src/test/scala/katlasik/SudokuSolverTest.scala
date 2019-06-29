package katlasik

import org.scalatest.{FlatSpec, Matchers}


class SudokuSolverTest extends FlatSpec with Matchers {

  "solve" should "resolve correctly sudoku" in {

    val input = "___26_7_168__7__9_19___45__82_1___4___46_29___5___3_28__93___74_4__5__367_3_18___"

    val expected = "435269781682571493197834562826195347374682915951743628519326874248957136763418259"

    SudokuSolver.solve(input) shouldEqual Some(expected)

  }

  "maybeNextCoordinates" should "return correct next coordinates" in {

    SudokuSolver.maybeNextCoordinates((8,8), 9) shouldEqual None

    SudokuSolver.maybeNextCoordinates((8,5), 9) shouldEqual Some(0,6)

    SudokuSolver.maybeNextCoordinates((6,8), 9) shouldEqual Some(7,8)

  }

  "updateBoard" should "return updated board" in {

    val board = Vector(
      Vector('_', '_', '_'),
      Vector('2', '6', '_'),
      Vector('7', '_', '1')
    )

    val expected =


    SudokuSolver.updateBoard(board, (1,0), '5') shouldEqual Vector(
      Vector('_', '5', '_'),
      Vector('2', '6', '_'),
      Vector('7', '_', '1')
    )

    SudokuSolver.updateBoard(board, (1,2), '5') shouldEqual Vector(
      Vector('_', '_', '_'),
      Vector('2', '6', '_'),
      Vector('7', '5', '1')
    )

  }

  "partition" should "do correct partitioning" in {

    val input = "___26_7_1"

    val expected = Vector(
      Vector('_', '_', '_'),
      Vector('2', '6', '_'),
      Vector('7', '_', '1')
    )

    SudokuSolver.partition(input.iterator, 3) shouldEqual expected

  }

  "checkAvailability" should "get available numbers" in {

    val board = Vector(
      Vector('4', '3', '_', '2', '6', '_', '7', '_', '1'),
      Vector('6', '8', '_', '_', '7', '_', '_', '9', '_'),
      Vector('1', '9', '_', '_', '_', '4', '5', '_', '_'),
      Vector('8', '2', '_', '1', '_', '_', '_', '4', '_'),
      Vector('_', '_', '4', '6', '_', '2', '9', '_', '_'),
      Vector('_', '5', '_', '_', '_', '3', '_', '2', '8'),
      Vector('_', '_', '9', '3', '_', '_', '_', '7', '4'),
      Vector('_', '4', '_', '_', '5', '_', '_', '3', '6'),
      Vector('7', '_', '3', '_', '1', '8', '_', '_', '_')
    )

    SudokuSolver.availableCharacters(board, (2,0)) shouldEqual Set('5')

    SudokuSolver.availableCharacters(board, (8,8)) shouldEqual Set('9', '5', '2')

  }

}
