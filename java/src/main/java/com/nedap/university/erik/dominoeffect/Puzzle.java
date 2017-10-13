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

  private static String pad(Integer integer, int maxPips) {
    String s = integer.toString();
    int n = 2 + ((int) Math.floor(Math.log10(2 * Bone.totalNumber(maxPips))));
    int m = n - s.length();
    return s + String.join("", Collections.nCopies(m, " "));
  }

  public String print(int maxPips) {
    List<Puzzle> rows = chop(calculateMaxColumnIndex(maxPips) + 1);
    return String.join(
        "", rows.stream().map(puzzle -> puzzle.showRow(maxPips)).collect(Collectors.toList()));
  }

  private List<Puzzle> chop(int n) {
    List<Position> positions = new ArrayList<>(keySet());
    ArrayList<NavigableMap<Position, Integer>> navigableChops = new ArrayList<>();
    NavigableMap<Position, Integer> tailMap = new TreeMap<>(this);
    int m = n;
    while (size() > n) {
      Position chopKey = positions.get(n);
      navigableChops.add(tailMap.headMap(chopKey, false));
      tailMap = tailMap.tailMap(chopKey, true);
      n += m;
    }
    navigableChops.add(tailMap);
    return navigableChops
        .stream()
        .map(map -> new Puzzle(new ArrayList<>(map.keySet()), new ArrayList<>(map.values())))
        .collect(Collectors.toCollection(ArrayList::new));
  }

  private String showRow(int maxPips) {
    return String.join(
            "",
            values()
                .stream()
                .map(integer -> Puzzle.pad(integer, maxPips))
                .collect(Collectors.toList()))
        + "\n";
  }
}
