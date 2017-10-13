package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.NavigableMap;
import java.util.TreeMap;
import java.util.stream.Collectors;

/** Created by erik.huizinga on 13-10-17 */
public class Puzzle extends TreeMap<Position, Integer> {
  private final int maxColumnIndex;
  private final int maxRowIndex;
  private final int maxIndex;

  public Puzzle(List<Position> positions, List<Integer> values) {
    int bound = positions.size();
    for (int i = 0; i < bound; i++) {
      put(positions.get(i), values.get(i));
    }
    maxColumnIndex = Position.maxColumnIndex(positions);
    maxRowIndex = Position.maxRowIndex(positions);
    maxIndex = Position.maxIndex(positions);
  }

  public int getMaxColumnIndex() {
    return maxColumnIndex;
  }

  public int getMaxRowIndex() {
    return maxRowIndex;
  }

  public int getMaxIndex() {
    return maxIndex;
  }

  public static int calculateMaxColumnIndex(int maxPips) {
    return calculateMaxRowIndex(maxPips) + 1;
  }

  public static int calculateMaxRowIndex(int maxPips) {
    return maxPips;
  }
}
