package com.nedap.university.erik.dominoeffect;

import java.util.Collections;
import java.util.Map;

/** Created by erik.huizinga on 14-10-17 */
public class Move {
  private final Map<Position, Integer> map1;
  private final Map<Position, Integer> map2;
  private final Position position1;
  private final Position position2;
  private final int value1;
  private final int value2;

  public Move(Position position1, Position position2, int value1, int value2) {
    this.position1 = position1;
    this.position2 = position2;
    this.value1 = value1;
    this.value2 = value2;
    map1 = Collections.singletonMap(position1, value1);
    map2 = Collections.singletonMap(position2, value2);
  }

  public Position getPosition1() {
    return position1;
  }

  public Position getPosition2() {
    return position2;
  }

  public int getValue1() {
    return value1;
  }

  public int getValue2() {
    return value2;
  }

  @Override
  public String toString() {
    return "{p1: " + position1 + ", p2: " + position2 + ", v1: " + value1 + ", v2: " + value2 + "}";
  }
}
