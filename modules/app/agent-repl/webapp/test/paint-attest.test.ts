/**
 * paint-attest.ts — this frontend's answer for one render pass (F5).
 *
 * The cases below pin the two facts the daemon's delivery sequencer rests on:
 * a render that drew reports PAINTED for the generation it drew, and a webview
 * that cannot draw reports SUSPENDED instead of going silent.
 */
import { describe, expect, it } from "vitest";

import type { PaintOutcome } from "../src/frontend-command.js";
import { PaintAttestation } from "../src/paint-attest.js";

interface Sent {
  throughSeq: number;
  generation: number;
  outcome: PaintOutcome;
}

/** A harness whose sends are recorded and resolve immediately. */
function harness(canPaint: () => boolean = () => true) {
  const sent: Sent[] = [];
  const logged: string[] = [];
  const attest = new PaintAttestation({
    send: (throughSeq, generation, outcome) => {
      sent.push({ throughSeq, generation, outcome });
      return Promise.resolve();
    },
    canPaint,
    log: (message) => logged.push(message),
  });
  return { attest, sent, logged };
}

/** A harness whose sends always reject, for the rollback cases. */
function failingHarness() {
  const sent: Sent[] = [];
  const logged: string[] = [];
  const attest = new PaintAttestation({
    send: (throughSeq, generation, outcome) => {
      sent.push({ throughSeq, generation, outcome });
      return Promise.reject(new Error("refused"));
    },
    canPaint: () => true,
    log: (message) => logged.push(message),
  });
  return { attest, sent, logged };
}

describe("PaintAttestation — painting", () => {
  it("reports the seq and the generation one render drew", () => {
    // Arrange
    const { attest, sent } = harness();
    // Act
    attest.painted({ throughSeq: 4, generation: 2 });
    // Assert
    expect(sent).toEqual([{ throughSeq: 4, generation: 2, outcome: "painted" }]);
  });

  it("attests an empty history at seq 0, which is a real claim", () => {
    // Arrange — "there was nothing to draw and I drew it" is what lets a
    // never-prompted session reach ready.
    const { attest, sent } = harness();
    // Act
    attest.painted({ throughSeq: 0, generation: 1 });
    // Assert
    expect(sent).toEqual([{ throughSeq: 0, generation: 1, outcome: "painted" }]);
  });

  it("does not re-send when neither identity moved", () => {
    // Arrange
    const { attest, sent } = harness();
    attest.painted({ throughSeq: 4, generation: 2 });
    // Act — the same pass renders again with nothing new.
    attest.painted({ throughSeq: 4, generation: 2 });
    // Assert
    expect(sent).toHaveLength(1);
  });

  it("re-sends when only the STATE generation moved", () => {
    // Arrange — a state change with no new conversation is exactly the case
    // that must still be answered, or the tab bar would never be released.
    const { attest, sent } = harness();
    attest.painted({ throughSeq: 4, generation: 2 });
    // Act
    attest.painted({ throughSeq: 4, generation: 3 });
    // Assert
    expect(sent).toHaveLength(2);
    expect(sent[1]).toEqual({ throughSeq: 4, generation: 3, outcome: "painted" });
  });

  it("re-sends when only the conversation seq moved", () => {
    // Arrange
    const { attest, sent } = harness();
    attest.painted({ throughSeq: 4, generation: 2 });
    // Act
    attest.painted({ throughSeq: 9, generation: 2 });
    // Assert
    expect(sent).toHaveLength(2);
    expect(sent[1]).toEqual({ throughSeq: 9, generation: 2, outcome: "painted" });
  });
});

describe("PaintAttestation — a webview that cannot draw", () => {
  it("answers SUSPENDED for a state it received and cannot paint", () => {
    // Arrange — a hidden webview receives frames normally and has no animation
    // frame to render them in.
    const { attest, sent } = harness(() => false);
    // Act
    attest.observe({ throughSeq: 4, generation: 2 });
    // Assert
    expect(sent).toEqual([{ throughSeq: 4, generation: 2, outcome: "suspended" }]);
  });

  it("stays silent while it CAN paint, leaving the render to report", () => {
    // Arrange
    const { attest, sent } = harness(() => true);
    // Act
    attest.observe({ throughSeq: 4, generation: 2 });
    // Assert
    expect(sent).toHaveLength(0);
  });

  it("answers each generation once rather than on every frame", () => {
    // Arrange
    const { attest, sent } = harness(() => false);
    attest.observe({ throughSeq: 4, generation: 2 });
    // Act — more conversation arrives, but no new state.
    attest.observe({ throughSeq: 9, generation: 2 });
    // Assert
    expect(sent).toHaveLength(1);
  });

  it("answers a NEW generation that arrives while it is still hidden", () => {
    // Arrange
    const { attest, sent } = harness(() => false);
    attest.observe({ throughSeq: 4, generation: 2 });
    // Act
    attest.observe({ throughSeq: 4, generation: 3 });
    // Assert
    expect(sent).toHaveLength(2);
    expect(sent[1].outcome).toBe("suspended");
  });

  it("still sends the PAINTED attestation for a generation it settled while hidden", () => {
    // Arrange — THE repaint-on-show case. A suspended answer settles delivery
    // and attests nothing, so the repaint that follows must still be able to
    // make the attestation the workspace's green depends on.
    let visible = false;
    const { attest, sent } = harness(() => visible);
    attest.observe({ throughSeq: 4, generation: 2 });
    // Act — the webview becomes visible and repaints the same snapshot.
    visible = true;
    attest.painted({ throughSeq: 4, generation: 2 });
    // Assert
    expect(sent).toHaveLength(2);
    expect(sent[1]).toEqual({ throughSeq: 4, generation: 2, outcome: "painted" });
  });
});

describe("PaintAttestation — a send that fails", () => {
  it("re-attests rather than bookkeeping an ack it did not land", async () => {
    // Arrange — the failure mode the whole mechanism guards against is a
    // frontend that LOOKS attested without having drawn.
    const { attest, sent } = failingHarness();
    attest.painted({ throughSeq: 4, generation: 2 });
    await Promise.resolve();
    await Promise.resolve();
    // Act — the same snapshot renders again.
    attest.painted({ throughSeq: 4, generation: 2 });
    // Assert
    expect(sent).toHaveLength(2);
  });

  it("surfaces the failure rather than swallowing it", async () => {
    // Arrange
    const { attest, logged } = failingHarness();
    // Act
    attest.painted({ throughSeq: 4, generation: 2 });
    await Promise.resolve();
    await Promise.resolve();
    // Assert
    expect(logged).toHaveLength(1);
    expect(logged[0]).toContain("painted");
  });
});
