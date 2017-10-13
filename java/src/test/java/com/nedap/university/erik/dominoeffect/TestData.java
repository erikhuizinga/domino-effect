package com.nedap.university.erik.dominoeffect;

import java.util.ArrayList;
import java.util.Arrays;
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
  }
}
