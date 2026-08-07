/**
 * The gate on session-addressed side-calls.
 *
 * The defect it exists to close: a workspace-addressed page fired the account
 * lookup at mount, before the daemon had said which session the workspace
 * owns, so every mount issued `GET /sessions//account` and logged the 404 it
 * was always going to get.
 */
import { describe, expect, it } from "vitest";

import { SessionIdentityGate } from "../src/session-identity.js";

/** A page whose session id can be rebound the way the pushed plane rebinds it. */
function page(initial = ""): { gate: SessionIdentityGate; bind: (id: string) => void } {
  let sessionId = initial;
  const gate = new SessionIdentityGate(() => sessionId);
  return {
    gate,
    bind: (id: string) => {
      sessionId = id;
      gate.announce();
    },
  };
}

describe("SessionIdentityGate", () => {
  it("runs no side-call while the page has no session id", () => {
    // Arrange
    const { gate } = page();
    const targeted: string[] = [];
    // Act
    gate.whenBound((id) => targeted.push(id));
    // Assert
    expect(targeted).toEqual([]);
  });

  it("runs the side-call once the identity binds", () => {
    // Arrange
    const { gate, bind } = page();
    const targeted: string[] = [];
    gate.whenBound((id) => targeted.push(id));
    // Act
    bind("s_1");
    // Assert
    expect(targeted).toEqual(["s_1"]);
  });

  it("runs the side-call immediately when the page mounts already addressed", () => {
    // Arrange
    const { gate } = page("s_1");
    const targeted: string[] = [];
    // Act
    gate.whenBound((id) => targeted.push(id));
    // Assert
    expect(targeted).toEqual(["s_1"]);
  });

  it("re-runs the side-call against a rebound session", () => {
    // Arrange
    const { gate, bind } = page("s_1");
    const targeted: string[] = [];
    gate.whenBound((id) => targeted.push(id));
    // Act
    bind("s_2");
    // Assert
    expect(targeted).toEqual(["s_1", "s_2"]);
  });

  it("announces nothing while the id is still empty", () => {
    // Arrange
    const { gate, bind } = page();
    const targeted: string[] = [];
    gate.whenBound((id) => targeted.push(id));
    // Act
    bind("");
    // Assert
    expect(targeted).toEqual([]);
  });
});
