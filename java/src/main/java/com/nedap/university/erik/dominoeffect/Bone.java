package com.nedap.university.erik.dominoeffect;

import static com.nedap.university.erik.dominoeffect.Puzzle.NO_PIPS;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/** Created by erik.huizinga on 13-10-17 */
public class Bone implements Map<Integer, List<Integer>>, Comparable {

  private static final Map<Integer, Integer> totalNumberMaxPipsMap =
      Map.of(
          totalNumber(0),
          0,
          totalNumber(1),
          1,
          totalNumber(2),
          2,
          totalNumber(3),
          3,
          totalNumber(4),
          4,
          totalNumber(5),
          5,
          totalNumber(6),
          6);
  private final Map<Integer, List<Integer>> map;
  private final int boneNumber;
  private final int pips0;
  private final int pips1;

  public Bone(Integer pips0, Integer pips1, int boneNumber) {
    map = Collections.singletonMap(boneNumber, List.of(pips0, pips1));
    this.pips0 = pips0;
    this.pips1 = pips1;
    this.boneNumber = boneNumber;
  }

  public int getBoneNumber() {
    return boneNumber;
  }

  public int getPips0() {
    return pips0;
  }

  public int getPips1() {
    return pips1;
  }

  public static int totalNumber(int maxPips) {
    return IntStream.rangeClosed(1, maxPips + 1).sum();
  }

  public static int maxPips(int totalNumber) {
    Integer maxPips = totalNumberMaxPipsMap.get(totalNumber);
    return maxPips == null ? NO_PIPS : maxPips;
  }

  public static SortedSet<Bone> initialSetOf(int maxPips) {
    SortedSet<Bone> bones = new TreeSet<>();
    int boneNumber = 1;
    for (Integer pips0 = 0; pips0 <= maxPips; pips0++) {
      for (Integer pips1 = pips0; pips1 <= maxPips; pips1++) {
        bones.add(new Bone(pips0, pips1, boneNumber));
        boneNumber++;
      }
    }
    return bones;
  }

  public static String print(Collection<Bone> bones, int maxPips) {
    return String.join(
        "\n", bones.stream().map(bone -> bone.print(maxPips)).collect(Collectors.toList()));
  }

  public static Set<Bone> filterBones(Set<Bone> bones, Move move) {
    TreeSet<Bone> bones1 =
        new TreeSet<>(
            bones
                .stream()
                .filter(bone -> bone.boneNumber != move.getValue1())
                .filter(bone -> bone.boneNumber != move.getValue2())
                .collect(Collectors.toSet()));
    return bones1;
  }

  public String print(int maxPips) {
    return "#" + Tools.pad(boneNumber, maxPips) + pips0 + "|" + pips1;
  }

  public Bone boneWithPipsFirst(int pips) {
    return hasPrimaryPips(pips) ? this : new Bone(pips, pips0, boneNumber);
  }

  public boolean hasPips(int pips) {
    return hasPrimaryPips(pips) || hasSecondaryPips(pips);
  }

  public boolean hasPrimaryPips(int pips) {
    return pips0 == pips;
  }

  public boolean hasSecondaryPips(int pips) {
    return pips1 == pips;
  }

  @Override
  public int compareTo(Object obj) {
    if (obj instanceof Bone) {
      Bone that = (Bone) obj;
      return boneNumber - that.boneNumber;
    }
    throw new ClassCastException("Not a Bone");
  }

  @Override
  public int size() {
    return map.size();
  }

  @Override
  public boolean isEmpty() {
    return map.isEmpty();
  }

  @Override
  public boolean containsKey(Object key) {
    return map.containsKey(key);
  }

  @Override
  public boolean containsValue(Object value) {
    return map.containsValue(value);
  }

  @Override
  public List<Integer> get(Object key) {
    return map.get(key);
  }

  @Override
  public List<Integer> put(Integer key, List<Integer> value) {
    return map.put(key, value);
  }

  @Override
  public List<Integer> remove(Object key) {
    return map.remove(key);
  }

  @Override
  public void putAll(Map<? extends Integer, ? extends List<Integer>> m) {
    map.putAll(m);
  }

  @Override
  public void clear() {
    map.clear();
  }

  @Override
  public Set<Integer> keySet() {
    return map.keySet();
  }

  @Override
  public Collection<List<Integer>> values() {
    return map.values();
  }

  @Override
  public Set<Entry<Integer, List<Integer>>> entrySet() {
    return map.entrySet();
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof Bone) {
      Bone that = (Bone) obj;
      return pips0 == that.pips0 && pips1 == that.pips1 && boneNumber == that.boneNumber;
    }
    return super.equals(obj);
  }
}
