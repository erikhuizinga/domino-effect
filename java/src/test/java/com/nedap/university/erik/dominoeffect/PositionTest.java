package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.Position.NO_INDEX;
import static com.nedap.university.erik.dominoeffect.TestData.pos0;
import static com.nedap.university.erik.dominoeffect.TestData.pos1;
import static com.nedap.university.erik.dominoeffect.TestData.pos2;
import static com.nedap.university.erik.dominoeffect.TestData.pos3;
import static com.nedap.university.erik.dominoeffect.TestData.pos4;
import static com.nedap.university.erik.dominoeffect.TestData.pos5;
import static com.nedap.university.erik.dominoeffect.TestData.positions;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Created by erik.huizinga on 13-10-17 */
class PositionTest {

  @BeforeEach
  void setUp() {
    TestData.reset();
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
  void initialSetOf() {
    assertEquals(Set.of(pos0, pos1), Position.initialSetOf(0));
    Set<Position> initialPositions = Position.initialSetOf(1);
    TestTools.checkEquivalence(positions, initialPositions);
    TestTools.checkEquivalence(initialPositions, positions);
  }

  @Test
  void compareTo() {
    assertTrue(pos0.compareTo(pos1) < 0);
    assertTrue(pos2.compareTo(pos2) == 0);
    assertTrue(pos4.compareTo(pos3) > 0);
  }
}
