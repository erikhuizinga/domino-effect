package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NavigableMap;
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
        "\n", rows.stream().map(puzzle -> puzzle.showRow(maxPips)).collect(Collectors.toList()));
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
    return findMoves(positions, bones)
        .stream()
        .map(
            move ->
                solve(
                    Position.filterPositions(positions, move),
                    Bone.filterBones(bones, move),
                    solution.apply(move)))
        .flatMap(Collection::stream)
        .collect(Collectors.toSet());
    //        .reduce(
    //            (solutions1, solutions2) -> {
    //              solutions1.addAll(solutions2);
    //              return solutions1;
    //            });
    //    return Collections.emptySet(); // TODO: Stub
  }

  private List<Move> findMoves(Set<Position> positions, Set<Bone> bones) {
    TreeSet<Position> sortedPositions = new TreeSet<>(positions);
    Position position = sortedPositions.first();
    int positionsPips = get(position);
    Collection<Position> neighbourPositions =
        position.neighbours(sortedPositions.tailSet(position));
    return neighbourPositions
        .stream()
        .map(
            neighbourPosition -> // For each neighbour position...
            bones
                    .stream()
                    // ... find bones with matching pips at this and that position ...
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
        // ... and join these lists of moves for all neighbours in a single list ...
        .reduce(
            (moves1, moves2) -> {
              moves1.addAll(moves2);
              return moves1;
            })
        // ... but if there are none, return an empty list
        .orElse(Collections.emptyList());
  }
}
