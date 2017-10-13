package com.nedap.university.erik.dominoeffect;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;

/** Created by erik.huizinga on 13-10-17 */
class PuzzleTest {

  private Position pos000 = new Position(0, 0, 0);
  private Position pos011 = new Position(0, 1, 1);
  private Position pos102 = new Position(1, 0, 2);
  private Position pos113 = new Position(1, 1, 3);
  private ArrayList<Position> positions =
      new ArrayList<>(Arrays.asList(pos000, pos011, pos102, pos113));
  private List<Integer> values = new ArrayList<>(Arrays.asList(0, 0, 0, 0));
  public Puzzle puzzle = new Puzzle(positions, values);

  @Test
  void getMaxColumnIndex() {
    assertEquals(1, puzzle.getMaxColumnIndex());
  }

  @Test
  void getMaxRowIndex() {
    assertEquals(1, puzzle.getMaxRowIndex());
  }

  @Test
  void getMaxIndex() {
    assertEquals(3, puzzle.getMaxIndex());
  }

  @Test
  void Puzzle() {
    assertTrue(values.containsAll(puzzle.values()));
    assertTrue(positions.containsAll(puzzle.keySet()));
  }

  @Test
  void calculateMaxRowIndex() {
    assertEquals(1, Puzzle.calculateMaxRowIndex(1));
  }

  @Test
  void calculateMaxColumnIndex() {
    assertEquals(2, Puzzle.calculateMaxColumnIndex(1));
  }
}
