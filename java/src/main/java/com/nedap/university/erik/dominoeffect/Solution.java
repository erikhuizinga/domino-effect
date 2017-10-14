package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

/** Created by erik.huizinga on 13-10-17 */
public class Solution extends Puzzle {

  public static final int DEFAULT = 0;

  public Solution(Collection<Position> positions, Collection<Integer> values) {
    super(positions, values);
  }

  public boolean isSolved() {
    return !containsValue(DEFAULT);
  }

  public static Solution initializeFor(Puzzle puzzle) {
    List<Integer> values = new ArrayList<>(puzzle.values());
    values.replaceAll(integer -> DEFAULT);
    return new Solution(new ArrayList<>(puzzle.keySet()), values);
  }

  public static String print(Set<Solution> solutions) {
    return ""; // TODO: Stub
  }

  public Solution apply(Move move) {
    return this; // TODO: Stub
  }
}
