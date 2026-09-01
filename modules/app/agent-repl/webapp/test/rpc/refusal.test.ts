import { describe, expect, it } from "vitest";
import { SelectWorkspaceErrorSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { refusalSentence } from "../../src/rpc/refusal.js";
import { armsOf } from "../feed/arms.js";

describe("refusalSentence: the four cross-cutting causes are worded once", () => {
  it("says the workspace is not in the registry", () => {
    expect(refusalSentence("SelectWorkspace", { case: "unknownWorkspace", value: {} })).toMatch(
      /registry/,
    );
  });

  it("carries the registry's own dir on a ref mismatch", () => {
    expect(
      refusalSentence("SelectWorkspace", {
        case: "workspaceRefMismatch",
        value: { registryDir: "/elsewhere" },
      }),
    ).toContain("/elsewhere");
  });

  it("carries the successor's address when the daemon transferred it away", () => {
    expect(
      refusalSentence("SelectWorkspace", {
        case: "transferringAway",
        value: { address: "127.0.0.1:7" },
      }),
    ).toContain("127.0.0.1:7");
  });

  it("says adoption is unfinished for a joining daemon", () => {
    expect(refusalSentence("SelectWorkspace", { case: "notYetAdopted", value: {} })).toMatch(
      /adopting/,
    );
  });
});

describe("refusalSentence: a per-rpc arm is the call site's to word", () => {
  it("returns undefined for an arm it does not own", () => {
    expect(refusalSentence("CloseWorkspace", { case: "somePerRpcArm", value: {} })).toBeUndefined();
  });

  it("covers every arm SelectWorkspaceError declares", () => {
    for (const arm of armsOf(SelectWorkspaceErrorSchema.oneofs, "cause")) {
      expect(refusalSentence("SelectWorkspace", { case: arm, value: {} })).toBeDefined();
    }
  });
});
