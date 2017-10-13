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

  private Position pos000;
  private Position pos011;
  private Position pos102;
  private Position pos113;
  private Collection<Position> positions;

  @BeforeEach
  void setUp() {
    pos000 = new Position(0, 0, 0);
    pos011 = new Position(0, 1, 1);
    pos102 = new Position(1, 0, 2);
    pos113 = new Position(1, 1, 3);
    positions = new ArrayList<>(Arrays.asList(pos000, pos011, pos102, pos113));
  }

  @Test
  void neighbours() {
    List<Position> expectedNeighbours = new ArrayList<>(List.of(pos011, pos102));
    Collection<Position> neighbours = pos000.neighbours(positions);
    assertTrue(expectedNeighbours.containsAll(neighbours));
    expectedNeighbours.remove(pos011);
    positions.remove(pos011);
    neighbours = pos000.neighbours(positions);
    assertTrue(expectedNeighbours.containsAll(neighbours));
  }

  @Test
  void maxColumnIndex() {
    assertEquals(1, Position.maxColumnIndex(positions));
    positions.remove(pos113);
    assertEquals(1, Position.maxColumnIndex(positions));
    positions.remove(pos011);
    assertEquals(0, Position.maxColumnIndex(positions));
    positions.clear();
    assertEquals(NO_INDEX, Position.maxColumnIndex(positions));
  }

  @Test
  void maxRowIndex() {
    assertEquals(1, Position.maxRowIndex(positions));
    positions.remove(pos113);
    assertEquals(1, Position.maxRowIndex(positions));
    positions.remove(pos102);
    assertEquals(0, Position.maxRowIndex(positions));
    positions.clear();
    assertEquals(NO_INDEX, Position.maxRowIndex(positions));
  }

  @Test
  void maxIndex() {
    assertEquals(3, Position.maxIndex(positions));
    positions.remove(pos113);
    assertEquals(2, Position.maxIndex(positions));
    positions.remove(pos102);
    assertEquals(1, Position.maxIndex(positions));
    positions.clear();
    assertEquals(NO_INDEX, Position.maxIndex(positions));
  }
}
