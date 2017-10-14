package com.nedap.university.erik.dominoeffect;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/** Created by erik.huizinga on 13-10-17 */
public class TestData {

  public static Position pos0;
  public static Position pos1;
  public static Position pos2;
  public static Position pos3;
  public static Position pos4;
  public static Position pos5;
  public static int maxPips1;
  public static Set<Position> positions1;
  public static Puzzle puzzle1;
  public static List<Integer> values;
  public static Bone bone1;
  public static Bone bone2;
  public static Bone bone3;
  public static Set<Bone> bones1;
  public static Puzzle emptyPuzzle;
  public static Move move0112;

  static void reset() {
    pos0 = new Position(0, 0, 0);
    pos1 = new Position(0, 1, 1);
    pos2 = new Position(0, 2, 2);
    pos3 = new Position(1, 0, 3);
    pos4 = new Position(1, 1, 4);
    pos5 = new Position(1, 2, 5);
    maxPips1 = 1;
    positions1 = new TreeSet<>(Set.of(pos0, pos1, pos2, pos3, pos4, pos5));
    values = List.of(0, 0, 0, 1, 1, 1);
    puzzle1 = new Puzzle(positions1, values);
    bone1 = new Bone(0, 0, 1);
    bone2 = new Bone(0, 1, 2);
    bone3 = new Bone(1, 1, 3);
    bones1 = new TreeSet<>(Set.of(bone1, bone2, bone3));
    emptyPuzzle = new Puzzle(Collections.emptyList(), Collections.emptyList());
    move0112 = new Move(pos0, pos1, 1, 2);
  }
}
