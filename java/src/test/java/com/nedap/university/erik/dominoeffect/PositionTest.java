package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.Position.NO_INDEX;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Created by erik.huizinga on 13-10-17 */
class PositionTest {

  private Position pos0;
  private Position pos1;
  private Position pos2;
  private Position pos3;
  private Collection<Position> positions;
  private Position pos4;
  private Position pos5;

  @BeforeEach
  void setUp() {
    pos0 = new Position(0, 0, 0);
    pos1 = new Position(0, 1, 1);
    pos2 = new Position(0, 2, 2);
    pos3 = new Position(1, 0, 3);
    pos4 = new Position(1, 1, 4);
    pos5 = new Position(1, 2, 5);
    positions = new ArrayList<>(Arrays.asList(pos0, pos1, pos2, pos3, pos4, pos5));
  }

  @Test
  void neighbours() {
    List<Position> expectedNeighbours = new ArrayList<>(List.of(pos1, pos3));
    Collection<Position> neighbours = pos0.neighbours(positions);
    assertTrue(expectedNeighbours.containsAll(neighbours));
    assertTrue(neighbours.containsAll(expectedNeighbours));
    expectedNeighbours.remove(pos1);
    positions.remove(pos1);
    neighbours = pos0.neighbours(positions);
    assertTrue(expectedNeighbours.containsAll(neighbours));
    assertTrue(neighbours.containsAll(expectedNeighbours));
  }

  @Test
  void maxColumnIndex() {
    assertEquals(2, Position.maxColumnIndex(positions));
    positions.remove(pos2);
    positions.remove(pos5);
    assertEquals(1, Position.maxColumnIndex(positions));
    positions.remove(pos1);
    positions.remove(pos4);
    assertEquals(0, Position.maxColumnIndex(positions));
    positions.clear();
    assertEquals(NO_INDEX, Position.maxColumnIndex(positions));
  }

  @Test
  void maxRowIndex() {
    assertEquals(1, Position.maxRowIndex(positions));
    positions.remove(pos3);
    assertEquals(1, Position.maxRowIndex(positions));
    positions.remove(pos4);
    positions.remove(pos5);
    assertEquals(0, Position.maxRowIndex(positions));
    positions.clear();
    assertEquals(NO_INDEX, Position.maxRowIndex(positions));
  }

  @Test
  void maxIndex() {
    assertEquals(5, Position.maxIndex(positions));
    positions.remove(pos5);
    assertEquals(4, Position.maxIndex(positions));
    positions.clear();
    assertEquals(NO_INDEX, Position.maxIndex(positions));
  }

  @Test
  void initialize() {
    assertEquals(List.of(pos0, pos1), Position.initialize(0));
    List<Position> initialPositions = Position.initialize(1);
    assertTrue(positions.containsAll(initialPositions));
    assertTrue(initialPositions.containsAll(positions));
  }
}
