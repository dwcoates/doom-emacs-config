/**
 * own-turns — the turns this page submitted, and nothing else.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { TurnIdSchema } from "../../../proto/gen/ts/conversation/v1/turn_pb";
import { forgetOwnTurns, isOwnTurn, rememberOwnTurn } from "../../src/composer/own-turns.js";

const turn = (value: string): ReturnType<typeof create<typeof TurnIdSchema>> =>
  create(TurnIdSchema, { value });

afterEach(() => {
  forgetOwnTurns();
});

describe("own turns", () => {
  it("claims a turn it was told this page minted", () => {
    // Arrange / Act
    rememberOwnTurn(turn("t1"));
    // Assert
    expect(isOwnTurn(turn("t1"))).toBe(true);
  });

  it("claims nothing before a submission", () => {
    // Arrange / Act / Assert
    expect(isOwnTurn(turn("t1"))).toBe(false);
  });

  it("does not claim another submitter's turn", () => {
    // Arrange
    rememberOwnTurn(turn("t1"));
    // Act / Assert
    expect(isOwnTurn(turn("t2"))).toBe(false);
  });

  it("holds every turn the page submitted, not only the last", () => {
    // Arrange
    rememberOwnTurn(turn("t1"));
    rememberOwnTurn(turn("t2"));
    // Act / Assert
    expect(isOwnTurn(turn("t1"))).toBe(true);
  });

  it("forgets every claim when the page is torn down", () => {
    // Arrange
    rememberOwnTurn(turn("t1"));
    // Act
    forgetOwnTurns();
    // Assert
    expect(isOwnTurn(turn("t1"))).toBe(false);
  });
});
