package com.nedap.university.erik.dominoeffect;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/** Created by erik.huizinga on 13-10-17 */
public class Bone implements Map<Integer, Set<Integer>>, Comparable {

  private final Map<Integer, Set<Integer>> map;
  private final int boneNumber;
  private final int pips0;
  private final int pips1;

  public Bone(Integer pips0, Integer pips1, int boneNumber) {
    // put(boneNumber, new HashSet<>(Arrays.asList(pips0, pips1)));
    map =
        Collections.singletonMap(
            boneNumber, Collections.unmodifiableSet(new HashSet<>(Arrays.asList(pips0, pips1))));
    this.pips0 = pips0;
    this.pips1 = pips1;
    this.boneNumber = boneNumber;
  }

  public static int totalNumber(int maxPips) {
    return IntStream.rangeClosed(1, maxPips + 1).sum();
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

  public String print(int maxPips) {
    return "#" + Tools.pad(boneNumber, maxPips) + pips0 + "|" + pips1;
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
  public Set<Integer> get(Object key) {
    return map.get(key);
  }

  @Override
  public Set<Integer> put(Integer key, Set<Integer> value) {
    return map.put(key, value);
  }

  @Override
  public Set<Integer> remove(Object key) {
    return map.remove(key);
  }

  @Override
  public void putAll(Map<? extends Integer, ? extends Set<Integer>> m) {
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
  public Collection<Set<Integer>> values() {
    return map.values();
  }

  @Override
  public Set<Entry<Integer, Set<Integer>>> entrySet() {
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