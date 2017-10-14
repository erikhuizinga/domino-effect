package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;

/** Created by erik.huizinga on 13-10-17 */
public class Solution extends Puzzle {

  public static final int DEFAULT = 0;

  public Solution(Collection<Position> positions, Collection<Integer> values) {
    super(positions, values);
  }

  private Solution(Puzzle puzzle) {
    this(
        puzzle.keySet(),
        new ArrayList<>(puzzle.values())
            .stream()
            .map(integer -> DEFAULT)
            .collect(Collectors.toList()));
  }

  public boolean isSolved() {
    return !containsValue(DEFAULT);
  }

  public static Solution initializeFor(Puzzle puzzle) {
    return new Solution(puzzle);
  }

  public Solution update(Move move) {
    Solution updated = new Solution(keySet(), values());
    updated.put(move.getPosition1(), move.getValue1());
    updated.put(move.getPosition2(), move.getValue2());
    return updated;
  }

  private int calculateMaxPips() {
    return Bone.maxPips(Collections.max(values()));
  }

  @Override
  public int getMaxPips() {
    return calculateMaxPips();
  }
}
