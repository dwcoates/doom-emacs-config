/**
 * The two history verbs ON THE WIRE.
 *
 * The client cannot name a position, so neither command has a field for one.
 * These cases pin that as an assertion about the encoded bytes rather than a
 * convention callers are supposed to observe. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { encodeFrontendCommand } from "../src/frontend-command.js";

function armOf(raw: string): { arm: string; body: Record<string, unknown> } {
  const cmd = JSON.parse(raw) as Record<string, unknown>;
  const arm = Object.keys(cmd).find((k) => k !== "requestId" && k !== "workspace");
  if (arm === undefined) throw new Error("no command arm encoded");
  return { arm, body: cmd[arm] as Record<string, unknown> };
}

describe("encoding the positionless history commands", () => {
  it("encodes FirstPageCmd under its own arm", () => {
    // Arrange / Act
    const raw = encodeFrontendCommand({
      requestId: "r1",
      workspace: "/ws/a",
      body: { case: "firstPage", workspace: "/ws/a" },
    });
    // Assert
    expect(armOf(raw).arm).toBe("firstPage");
  });

  it("encodes a FirstPageCmd carrying only the workspace", () => {
    // Arrange — no seq, no offset, no cursor, no fence.
    const raw = encodeFrontendCommand({
      requestId: "r1",
      workspace: "/ws/a",
      body: { case: "firstPage", workspace: "/ws/a" },
    });
    // Act
    const { body } = armOf(raw);
    // Assert
    expect(body).toEqual({ workspace: "/ws/a" });
  });

  it("encodes a NextPageCmd carrying NO POSITION", () => {
    // Arrange — the absence is the design: the daemon owns the position.
    const raw = encodeFrontendCommand({
      requestId: "r2",
      workspace: "/ws/a",
      body: { case: "nextPage", workspace: "/ws/a" },
    });
    // Act
    const { arm, body } = armOf(raw);
    // Assert
    expect(arm).toBe("nextPage");
    expect(Object.keys(body)).toEqual(["workspace"]);
  });

  it("never encodes a from_seq alongside a history verb", () => {
    // Arrange — the full-replay door is not what a cold open uses.
    const raw = encodeFrontendCommand({
      requestId: "r3",
      workspace: "/ws/a",
      body: { case: "firstPage", workspace: "/ws/a" },
    });
    // Act / Assert
    expect(raw).not.toContain("fromSeq");
  });
});
