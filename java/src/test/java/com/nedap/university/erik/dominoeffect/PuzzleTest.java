package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.Puzzle.NO_PIPS;
import static com.nedap.university.erik.dominoeffect.TestData.emptyPuzzle;
import static com.nedap.university.erik.dominoeffect.TestData.maxPips;
import static com.nedap.university.erik.dominoeffect.TestData.positions;
import static com.nedap.university.erik.dominoeffect.TestData.puzzle;
import static com.nedap.university.erik.dominoeffect.TestData.values;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Created by erik.huizinga on 13-10-17 */
class PuzzleTest {

  @BeforeEach
  void setUp() {
    TestData.reset();
  }

  @Test
  void getMaxColumnIndex() {
    assertEquals(2, puzzle.getMaxColumnIndex());
  }

  @Test
  void getMaxRowIndex() {
    assertEquals(1, puzzle.getMaxRowIndex());
  }

  @Test
  void getMaxIndex() {
    assertEquals(5, puzzle.getMaxIndex());
  }

  @Test
  void Puzzle() {
    Set<Position> puzzlePositions = puzzle.keySet();
    assertTrue(positions.containsAll(puzzlePositions));
    assertTrue(puzzlePositions.containsAll(positions));
    Collection<Integer> puzzleValues = puzzle.values();
    assertTrue(puzzleValues.containsAll(values));
    assertTrue(values.containsAll(puzzleValues));
  }

  @Test
  void getMaxPips() {
    assertEquals(NO_PIPS, emptyPuzzle.getMaxPips());
    assertEquals(6, Puzzles.assignment1.getMaxPips());
  }

  @Test
  void calculateMaxRowIndex() {
    assertEquals(1, Puzzle.calculateMaxRowIndex(1));
  }

  @Test
  void calculateMaxColumnIndex() {
    assertEquals(2, Puzzle.calculateMaxColumnIndex(1));
  }

  @Test
  void print() {
    assertEquals("0 0 0 \n" + "0 0 0 \n", puzzle.print(maxPips));
    assertEquals("0 0 \n" + "0 0 \n" + "0 0 \n", puzzle.print(0));
    assertEquals("0  0  0  0  0  0  \n", puzzle.print(6));
  }

  @Test
  void solve() {
    assertEquals(
        Collections.emptyList(),
        emptyPuzzle.solve(
            Collections.emptySet(), Collections.emptySet(), Solution.initializeFor(emptyPuzzle)));
    int maxPips = 6;
    assertTrue(
        Puzzles.assignment1
            .solve(
                Position.initialSetOf(maxPips),
                Bone.initialSetOf(maxPips),
                Solution.initializeFor(Puzzles.assignment1))
            .contains(Solutions.solution1_1));
  }
}
