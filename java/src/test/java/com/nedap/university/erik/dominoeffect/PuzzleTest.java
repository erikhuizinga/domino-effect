package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.Puzzle.NO_PIPS;
import static com.nedap.university.erik.dominoeffect.TestData.emptyPuzzle;
import static com.nedap.university.erik.dominoeffect.TestData.positions1;
import static com.nedap.university.erik.dominoeffect.TestData.puzzle1;
import static com.nedap.university.erik.dominoeffect.TestData.values;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
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
    assertEquals(2, puzzle1.getMaxColumnIndex());
  }

  @Test
  void getMaxRowIndex() {
    assertEquals(1, puzzle1.getMaxRowIndex());
  }

  @Test
  void getMaxIndex() {
    assertEquals(5, puzzle1.getMaxIndex());
  }

  @Test
  void Puzzle() {
    Set<Position> puzzlePositions = puzzle1.keySet();
    assertTrue(positions1.containsAll(puzzlePositions));
    assertTrue(puzzlePositions.containsAll(positions1));
    Collection<Integer> puzzleValues = puzzle1.values();
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
    assertEquals("0 0 0 \n" + "1 1 1 ", puzzle1.print());
    assertEquals(
        "0 0 0 \n" + "1 1 1 \n\n" + "0 0 0 \n" + "1 1 1 ",
        Puzzles.print(List.of(puzzle1, puzzle1)));
  }

  @Test
  void solve() {
    Solution emptySolution = Solution.initializeFor(emptyPuzzle);
    assertEquals(
        Set.of(emptySolution),
        emptyPuzzle.solve(new TreeSet<>(), Collections.emptySet(), emptySolution));
    assertEquals(Set.of(Solutions.solution1_1), puzzle1.solve(null, null, Solutions.solution1_1));

    int maxPips = Puzzles.assignment1.getMaxPips();
    Set<Solution> solutions =
        Puzzles.assignment1.solve(
            Position.initialSetOf(maxPips),
            Bone.initialSetOf(maxPips),
            Solution.initializeFor(Puzzles.assignment1));

    //    assertTrue(solutions.contains(Solutions.solution1_1));
    boolean found = false;
    for (Solution solution : solutions) {
      if (solution.equals(Solutions.solution1_1)) {
        found = true;
        break;
      }
    }
    if (!found) {
      fail("Expected solution not found!");
    }
  }
}
