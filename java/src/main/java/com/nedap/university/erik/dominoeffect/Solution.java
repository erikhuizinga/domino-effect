package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.List;

/** Created by erik.huizinga on 13-10-17 */
public class Solution extends Puzzle {

  public static final int DEFAULT = 0;

  public Solution(List<Position> positions, List<Integer> values) {
    super(positions, values);
  }

  public boolean isSolved() {
    return !containsValue(DEFAULT);
  }

  public static Solution initialize(Puzzle puzzle) {
    List<Integer> values = new ArrayList<>(puzzle.values());
    values.replaceAll(integer -> DEFAULT);
    return new Solution(new ArrayList<>(puzzle.keySet()), values);
  }
}
