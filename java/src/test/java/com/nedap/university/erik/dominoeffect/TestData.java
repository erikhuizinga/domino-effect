package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/** Created by erik.huizinga on 13-10-17 */
public class TestData {

  public static Position pos0;
  public static Position pos1;
  public static Position pos2;
  public static Position pos3;
  public static Position pos4;
  public static Position pos5;
  public static int maxPips;
  public static List<Position> positions;
  public static Puzzle puzzle;
  public static List<Integer> values;
  public static Bone bone1;
  public static Bone bone2;
  public static Bone bone3;
  public static Collection<Bone> bones1 = new LinkedList<>();
  public static Puzzle emptyPuzzle;

  static void reset() {
    pos0 = new Position(0, 0, 0);
    pos1 = new Position(0, 1, 1);
    pos2 = new Position(0, 2, 2);
    pos3 = new Position(1, 0, 3);
    pos4 = new Position(1, 1, 4);
    pos5 = new Position(1, 2, 5);
    maxPips = 1;
    positions = new ArrayList<>(Arrays.asList(pos0, pos1, pos2, pos3, pos4, pos5));
    values = new ArrayList<>(Arrays.asList(0, 0, 0, 0, 0, 0));
    puzzle = new Puzzle(positions, values);
    bone1 = new Bone(0, 0, 1);
    bone2 = new Bone(0, 1, 2);
    bone3 = new Bone(1, 1, 3);
    bones1.clear();
    bones1.add(bone1);
    bones1.add(bone2);
    bones1.add(bone3);
    emptyPuzzle = new Puzzle(Collections.emptyList(), Collections.emptyList());
  }
}
