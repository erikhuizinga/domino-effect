package com.nedap.university.erik.dominoeffect;

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.stream.Collectors;

/** Created by erik.huizinga on 13-10-17 */
public class Position implements Comparable {
  public static final int NO_INDEX = -1;
  private final int rowIndex;
  private final int columnIndex;
  private int index;

  public Position(int rowIndex, int columnIndex, int index) {
    this.rowIndex = rowIndex;
    this.columnIndex = columnIndex;
    this.index = index;
  }

  public int getRowIndex() {
    return rowIndex;
  }

  public int getColumnIndex() {
    return columnIndex;
  }

  public int getIndex() {
    return index;
  }

  public static int maxColumnIndex(Collection<Position> positions) {
    return maxIndexFunction(positions, Position::getColumnIndex);
  }

  public static int maxRowIndex(Collection<Position> positions) {
    return maxIndexFunction(positions, Position::getRowIndex);
  }

  public static int maxIndex(Collection<Position> positions) {
    return maxIndexFunction(positions, Position::getIndex);
  }

  private static int maxIndexFunction(
      Collection<Position> positions, Function<Position, Integer> indexFunction) {
    return positions.isEmpty()
        ? NO_INDEX
        : Collections.max(positions.stream().map(indexFunction).collect(Collectors.toSet()));
  }

  static TreeSet<Position> initialSetOf(int maxPips) {
    int maxColumnIndex = Puzzle.calculateMaxColumnIndex(maxPips);
    int maxRowIndex = Puzzle.calculateMaxRowIndex(maxPips);
    TreeSet<Position> positions = new TreeSet<>();
    for (int columnIndex = 0; columnIndex <= maxColumnIndex; columnIndex++) {
      for (int rowIndex = 0; rowIndex <= maxRowIndex; rowIndex++) {
        int index = columnIndex + rowIndex + rowIndex * maxColumnIndex;
        positions.add(new Position(rowIndex, columnIndex, index));
      }
    }
    return positions;
  }

  public static Set<Position> filterPositions(Set<Position> positions, Move move) {
    return new TreeSet<>(
        positions
            .stream()
            .filter(position -> !position.equals(move.getPosition1()))
            .filter(position -> !position.equals(move.getPosition2()))
            .collect(Collectors.toSet()));
  }

  public Collection<Position> neighbours(Collection<Position> positions) {
    Collection<Position> results =
        Set.of(
            new Position(rowIndex, columnIndex + 1, NO_INDEX),
            new Position(rowIndex + 1, columnIndex, NO_INDEX));
    results.forEach(
        result ->
            positions
                .stream()
                .filter(position -> position.rowIndex == result.rowIndex)
                .filter(position -> position.columnIndex == result.columnIndex)
                .findFirst()
                .ifPresent(position -> result.index = position.index));
    return results
        .stream()
        .filter(position -> position.index != NO_INDEX)
        .collect(Collectors.toList());
  }

  @Override
  public int compareTo(Object obj) {
    if (obj instanceof Position) {
      Position that = (Position) obj;
      return index - that.index;
    }
    throw new ClassCastException("Not a Position");
  }

  @Override
  public boolean equals(Object that) {
    if (that instanceof Position) {
      Position thatPosition = (Position) that;
      return thatPosition.rowIndex == rowIndex
          && thatPosition.columnIndex == columnIndex
          && thatPosition.index == index;
    }
    return super.equals(that);
  }

  @Override
  public String toString() {
    return "(" + rowIndex + ", " + columnIndex + ", " + index + ")";
  }

  @Override
  public int hashCode() {
    return Objects.hash(rowIndex, columnIndex, index);
  }
}
