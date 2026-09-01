import { describe, expect, it } from "vitest";
import { refusalSentence } from "../../src/rpc/refusal.js";

describe("refusalSentence", () => {
  it("words unknown_workspace once for every verb", () => {
    expect(refusalSentence("SetModel", { case: "unknownWorkspace", value: {} })).toBe(
      "unknown workspace",
    );
  });

  it("carries the registry's dir on workspace_ref_mismatch", () => {
    expect(
      refusalSentence("SetModel", {
        case: "workspaceRefMismatch",
        value: { registryDir: "/elsewhere" },
      }),
    ).toContain("/elsewhere");
  });

  it("carries the successor's address on transferring_away", () => {
    expect(
      refusalSentence("CloseLogin", { case: "transferringAway", value: { address: "127.0.0.1:9" } }),
    ).toContain("127.0.0.1:9");
  });

  it("words not_yet_adopted as the joining daemon's state", () => {
    expect(refusalSentence("OpenLogin", { case: "notYetAdopted", value: {} })).toBe(
      "the new daemon has not adopted this workspace yet",
    );
  });

  it("answers undefined for a per-rpc arm, which its call site words", () => {
    expect(refusalSentence("SetModel", { case: "notInCatalog", value: {} })).toBeUndefined();
  });
});
