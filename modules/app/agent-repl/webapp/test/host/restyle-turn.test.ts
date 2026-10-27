// A page's restyle turns: one ask while it waits, the latest work run in the
// turn, and the turn always ended. See src/host/restyle-turn.ts.
import { describe, expect, it } from "vitest";
import { createRestyleTurns, type RestyleTurnSend } from "../../src/host/restyle-turn.js";

/** Turns connected to a host that records what it is sent. */
function connected(): { turns: ReturnType<typeof createRestyleTurns>; sent: Parameters<RestyleTurnSend>[0][] } {
  const sent: Parameters<RestyleTurnSend>[0][] = [];
  const turns = createRestyleTurns();
  turns.connect((kind) => sent.push(kind));
  return { turns, sent };
}

describe("createRestyleTurns", () => {
  it("asks the host for a turn", () => {
    // Arrange
    const { turns, sent } = connected();
    // Act
    turns.request(() => {});
    // Assert
    expect(sent).toEqual(["restyleTurn"]);
  });

  it("asks once while it waits, and runs only the latest work in the turn", () => {
    // Arrange
    const { turns, sent } = connected();
    const ran: string[] = [];
    turns.request(() => ran.push("first"));
    turns.request(() => ran.push("second"));
    // Act
    turns.granted();
    // Assert
    expect([sent, ran]).toEqual([["restyleTurn", "restyled"], ["second"]]);
  });

  it("runs nothing it was not asked for", () => {
    // Arrange
    const { turns, sent } = connected();
    const ran: string[] = [];
    turns.request(() => ran.push("once"));
    turns.granted();
    // Act — a second grant with nothing waiting.
    turns.granted();
    // Assert
    expect([sent, ran]).toEqual([["restyleTurn", "restyled", "restyled"], ["once"]]);
  });

  it("asks again for work after its turn", () => {
    // Arrange
    const { turns, sent } = connected();
    turns.request(() => {});
    turns.granted();
    // Act
    turns.request(() => {});
    // Assert
    expect(sent).toEqual(["restyleTurn", "restyled", "restyleTurn"]);
  });

  it("asks again for work raised inside its own turn", () => {
    // Arrange
    const { turns, sent } = connected();
    turns.request(() => turns.request(() => {}));
    // Act
    turns.granted();
    // Assert
    expect(sent).toEqual(["restyleTurn", "restyleTurn", "restyled"]);
  });

  it("ends the turn even when its work throws, and throws on", () => {
    // Arrange
    const { turns, sent } = connected();
    turns.request(() => {
      throw new Error("boom");
    });
    // Act
    const act = (): void => turns.granted();
    // Assert
    expect(act).toThrow("boom");
    expect(sent).toEqual(["restyleTurn", "restyled"]);
  });

  it.each([
    ["an ask", (t: ReturnType<typeof createRestyleTurns>) => t.request(() => {})],
    ["a grant", (t: ReturnType<typeof createRestyleTurns>) => t.granted()],
  ])("refuses %s with no host link", (_name, act) => {
    // Arrange
    const turns = createRestyleTurns();
    // Act + Assert
    expect(() => act(turns)).toThrow(/restyle turn/);
  });
});
