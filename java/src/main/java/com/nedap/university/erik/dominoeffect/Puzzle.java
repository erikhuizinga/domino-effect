package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NavigableMap;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
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
    maxPips = calculateMaxPips();
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
    return positions.isEmpty() == bones.isEmpty();
  }

  private int calculateMaxPips() {
    return isEmpty() ? NO_PIPS : Collections.max(values());
  }

  public String print() {
    List<Puzzle> rows = chop(calculateMaxColumnIndex(getMaxPips()) + 1);
    return String.join(
        "\n",
        rows.stream().map(puzzle -> puzzle.showRow(getMaxPips())).collect(Collectors.toList()));
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
        values().stream().map(integer -> Tools.pad(integer, maxPips)).collect(Collectors.toList()));
  }

  public Set<Solution> solve(Set<Position> positions, Set<Bone> bones, Solution solution) {
    if (solution.isSolved()) {
      return Set.of(solution);
    }
    if (!okToContinue(positions, bones)) {
      return Collections.emptySet();
    }
    TreeSet<Position> sortedPositions = new TreeSet<>(positions);
    Position position = sortedPositions.first();
    int positionsPips = get(position);
    return position
        // Find the neighbours of this position ...
        .neighbours(sortedPositions.tailSet(position))
        .stream()
        .map(
            // ... and for each neighbour position ...
            neighbourPosition ->
                // ... and with the available bones ...
                bones
                    .stream()
                    // ... find any with matching pips at this and that position ...
                    .filter(bone -> bone.hasPips(positionsPips))
                    .map(bone -> bone.boneWithPipsFirst(positionsPips))
                    .filter(bone -> bone.hasSecondaryPips(get(neighbourPosition)))
                    // ... and get their bone numbers ...
                    .map(Bone::getBoneNumber)
                    // ... and construct moves...
                    .map(
                        boneNumber -> new Move(position, neighbourPosition, boneNumber, boneNumber))
                    // ... and store them in a list ...
                    .collect(Collectors.toList()))
        // ... and join these lists of moves in a single stream ...
        .flatMap(Collection::stream)
        .map(
            move ->
                // ... and solve again ...
                solve(
                    // ... using the remaining positions ...
                    Position.filterPositions(positions, move),
                    // ... and and the remaining bones ...
                    Bone.filterBones(bones, move),
                    // ... and the updated solution ...
                    solution.update(move)))
        // ... and collect these sets of solutions into one set
        .flatMap(Collection::stream)
        .collect(Collectors.toSet());
  }

  @Override
  public int hashCode() {
    return Objects.hash(maxPips, keySet(), values());
  }
}
