package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.Position.NO_INDEX;
import static com.nedap.university.erik.dominoeffect.TestData.move0112;
import static com.nedap.university.erik.dominoeffect.TestData.pos0;
import static com.nedap.university.erik.dominoeffect.TestData.pos1;
import static com.nedap.university.erik.dominoeffect.TestData.pos2;
import static com.nedap.university.erik.dominoeffect.TestData.pos3;
import static com.nedap.university.erik.dominoeffect.TestData.pos4;
import static com.nedap.university.erik.dominoeffect.TestData.pos5;
import static com.nedap.university.erik.dominoeffect.TestData.positions1;
import static org.junit.Assert.assertFalse;
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
    Collection<Position> neighbours = pos0.neighbours(positions1);
    assertTrue(expectedNeighbours.containsAll(neighbours));
    assertTrue(neighbours.containsAll(expectedNeighbours));
    expectedNeighbours.remove(pos1);
    positions1.remove(pos1);
    neighbours = pos0.neighbours(positions1);
    assertTrue(expectedNeighbours.containsAll(neighbours));
    assertTrue(neighbours.containsAll(expectedNeighbours));
  }

  @Test
  void maxColumnIndex() {
    assertEquals(2, Position.maxColumnIndex(positions1));
    positions1.remove(pos2);
    positions1.remove(pos5);
    assertEquals(1, Position.maxColumnIndex(positions1));
    positions1.remove(pos1);
    positions1.remove(pos4);
    assertEquals(0, Position.maxColumnIndex(positions1));
    positions1.clear();
    assertEquals(NO_INDEX, Position.maxColumnIndex(positions1));
  }

  @Test
  void maxRowIndex() {
    assertEquals(1, Position.maxRowIndex(positions1));
    positions1.remove(pos3);
    assertEquals(1, Position.maxRowIndex(positions1));
    positions1.remove(pos4);
    positions1.remove(pos5);
    assertEquals(0, Position.maxRowIndex(positions1));
    positions1.clear();
    assertEquals(NO_INDEX, Position.maxRowIndex(positions1));
  }

  @Test
  void maxIndex() {
    assertEquals(5, Position.maxIndex(positions1));
    positions1.remove(pos5);
    assertEquals(4, Position.maxIndex(positions1));
    positions1.clear();
    assertEquals(NO_INDEX, Position.maxIndex(positions1));
  }

  @Test
  void initialSetOf() {
    assertEquals(Set.of(pos0, pos1), Position.initialSetOf(0));
    Set<Position> initialPositions = Position.initialSetOf(1);
    TestTools.checkEquivalence(positions1, initialPositions);
    TestTools.checkEquivalence(initialPositions, positions1);
  }

  @Test
  void compareTo() {
    assertTrue(pos0.compareTo(pos1) < 0);
    assertTrue(pos2.compareTo(pos2) == 0);
    assertTrue(pos4.compareTo(pos3) > 0);
  }

  @Test
  void filterPositions() {
    Set<Position> positions = Position.filterPositions(positions1, move0112);
    assertFalse(positions.contains(pos0));
    assertFalse(positions.contains(pos1));
    assertTrue(positions1.containsAll(positions));
  }
}
