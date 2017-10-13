package com.nedap.university.erik.dominoeffect;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/** Created by erik.huizinga on 13-10-17 */
public class Position {
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

  public Collection<Position> neighbours(Collection<Position> positions) {
    Collection<Position> results =
        new HashSet<>(
            Set.of(
                new Position(rowIndex, columnIndex + 1, NO_INDEX),
                new Position(rowIndex + 1, columnIndex, NO_INDEX)));
    results.forEach(
        result ->
            positions
                .stream()
                .filter(position -> position.rowIndex == result.rowIndex)
                .filter(position -> position.columnIndex == result.columnIndex)
                .findFirst()
                .ifPresentOrElse(
                    position -> result.index = position.index, () -> result.index = NO_INDEX));
    return results
        .stream()
        .filter(position -> position.index != NO_INDEX)
        .collect(Collectors.toList());
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
}