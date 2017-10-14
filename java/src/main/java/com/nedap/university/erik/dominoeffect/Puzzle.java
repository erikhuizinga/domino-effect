package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

/** Created by erik.huizinga on 13-10-17 */
public class Puzzle extends TreeMap<Position, Integer> {

  public static final int NO_PIPS = -1;
  private final int maxColumnIndex;
  private final int maxRowIndex;
  private final int maxIndex;
  private final int maxPips;

  public Puzzle(Collection<Position> positions, Collection<Integer> values) {
    int bound = positions.size();
    ArrayList<Position> positionsList = new ArrayList<>(positions);
    ArrayList<Integer> valueList = new ArrayList<>(values);
    for (int i = 0; i < bound; i++) {
      put(positionsList.get(i), valueList.get(i));
    }
    maxColumnIndex = Position.maxColumnIndex(positions);
    maxRowIndex = Position.maxRowIndex(positions);
    maxIndex = Position.maxIndex(positions);
    maxPips = isEmpty() ? NO_PIPS : Collections.max(values);
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

  public int getMaxPips() {
    return maxPips;
  }

  public static int calculateMaxColumnIndex(int maxPips) {
    return calculateMaxRowIndex(maxPips) + 1;
  }

  public static int calculateMaxRowIndex(int maxPips) {
    return maxPips;
  }

  private static boolean okToContinue(Set<Position> positions, Set<Bone> bones) {
    return positions.isEmpty() ^ bones.isEmpty();
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
        .map(map -> new Puzzle(map.keySet(), map.values()))
        .collect(Collectors.toCollection(ArrayList::new));
  }

  private String showRow(int maxPips) {
    return String.join(
            "",
            values()
                .stream()
                .map(integer -> Tools.pad(integer, maxPips))
                .collect(Collectors.toList()))
        + "\n";
  }

  public Set<Solution> solve(Set<Position> positions, Set<Bone> bones, Solution solution) {
    if (solution.isSolved()) {
      return Set.of(solution);
    } else if (!okToContinue(positions, bones)) {
      return Collections.emptySet();
    }
    return Collections.emptySet(); // TODO: Stub
  }
}
