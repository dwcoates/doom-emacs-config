/**
 * fence — the ONE staleness gate every fenced component view passes through.
 *
 * The rule under test is small and absolute: byte-compare, adopt on equal,
 * discard WHOLE on anything else, and report every discard. One edge per test.
 */
import { describe, expect, it } from "vitest";
import { admitFenced, fencedFence, fencedWorkspace, type FencedView } from "../src/fence.js";

function topbar(fence: string, workspace = "/ws"): FencedView {
  return {
    case: "topbar",
    value: {
      workspace,
      title: "ws · main",
      sessionLine: "session line",
      modelDisplay: "opus-5",
      modelOptions: [],
      accountingLine: "",
      fence,
    },
  };
}

function gate(fence: string, workspace = "/ws"): FencedView {
  return {
    case: "workspaceGate",
    value: { workspace, fence, gate: { case: "open" } },
  };
}

function breakdown(fence: string, workspace = "/ws"): FencedView {
  return { case: "tokenBreakdown", value: { workspace, sections: [], fence } };
}

describe("the gate's verdict", () => {
  it("adopts a push whose fence equals the workspace's current fence", () => {
    // Arrange / Act
    const verdict = admitFenced(topbar("f1"), "f1");
    // Assert
    expect(verdict.kind).toBe("adopt");
  });

  it("hands the view back on adoption, so nothing else can be adopted instead", () => {
    // Arrange — the only reference a caller has to an admitted view comes out
    // of this function.
    const view = topbar("f1");
    // Act
    const verdict = admitFenced(view, "f1");
    // Assert
    expect(verdict.kind === "adopt" && verdict.view).toBe(view);
  });

  it("discards a push whose fence differs", () => {
    // Arrange / Act
    const verdict = admitFenced(topbar("f1"), "f2");
    // Assert
    expect(verdict.kind).toBe("discard");
  });

  it("discards when the store holds NO fence yet", () => {
    // Arrange / Act — nothing has established what current means for this
    // workspace, so no view can be shown to be current.
    const verdict = admitFenced(topbar("f1"), "");
    // Assert
    expect(verdict.kind).toBe("discard");
  });

  it("discards an EMPTY pushed fence against an empty current fence", () => {
    // Arrange / Act — two absences are not a match; they are two absences.
    const verdict = admitFenced(topbar(""), "");
    // Assert
    expect(verdict.kind).toBe("discard");
  });

  it("compares byte-wise rather than by prefix", () => {
    // Arrange / Act — the token's composition is the daemon's and free to
    // change, which is only true for as long as nothing reads structure into it.
    const verdict = admitFenced(topbar("f1"), "f1-extended");
    // Assert
    expect(verdict.kind).toBe("discard");
  });

  it("compares case-sensitively", () => {
    // Arrange / Act
    const verdict = admitFenced(topbar("F1"), "f1");
    // Assert
    expect(verdict.kind).toBe("discard");
  });
});

describe("the discard report", () => {
  it("names the view that was discarded", () => {
    // Arrange / Act — a silently dropped frame is indistinguishable from a
    // daemon that never sent one.
    const verdict = admitFenced(gate("f1"), "f2");
    // Assert
    expect(verdict.kind === "discard" && verdict.report.context.view).toBe("workspaceGate");
  });

  it("carries the pushed fence verbatim", () => {
    // Arrange / Act
    const verdict = admitFenced(breakdown("f1"), "f2");
    // Assert
    expect(verdict.kind === "discard" && verdict.report.context.pushed_fence).toBe("f1");
  });

  it("carries the current fence verbatim beside it", () => {
    // Arrange / Act — the whole diagnostic value is seeing that they differ.
    const verdict = admitFenced(breakdown("f1"), "f2");
    // Assert
    expect(verdict.kind === "discard" && verdict.report.context.current_fence).toBe("f2");
  });

  it("distinguishes a mismatch from having no ruling at all", () => {
    // Arrange / Act — a reader must be able to tell a rotation from a client
    // that never adopted a WorkspaceState.
    const verdict = admitFenced(topbar("f1"), "");
    // Assert
    expect(verdict.kind === "discard" && verdict.report.context.branch).toBe(
      "no_current_fence",
    );
  });

  it("brands a real mismatch as a fence mismatch", () => {
    // Arrange / Act
    const verdict = admitFenced(topbar("f1"), "f2");
    // Assert
    expect(verdict.kind === "discard" && verdict.report.context.branch).toBe("fence_mismatch");
  });

  it("says outright that no part of the push was adopted", () => {
    // Arrange / Act — the sentence is the contract: discarded WHOLE.
    const verdict = admitFenced(topbar("f1"), "f2");
    // Assert
    expect(verdict.kind === "discard" && verdict.report.message).toContain(
      "no part of it was adopted",
    );
  });

  it("names the workspace the discarded view belonged to", () => {
    // Arrange / Act
    const verdict = admitFenced(topbar("f1", "/other"), "f2");
    // Assert
    expect(verdict.kind === "discard" && verdict.report.context.workspace).toBe("/other");
  });
});

describe("the accessors every arm shares", () => {
  it("reads the workspace off a gate view", () => {
    // Arrange / Act / Assert
    expect(fencedWorkspace(gate("f1", "/w"))).toBe("/w");
  });

  it("reads the fence off a breakdown view", () => {
    // Arrange / Act / Assert
    expect(fencedFence(breakdown("f7"))).toBe("f7");
  });
});
