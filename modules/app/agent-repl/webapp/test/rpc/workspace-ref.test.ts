import { describe, expect, it } from "vitest";
import { workspaceRef } from "../../src/rpc/workspace-ref.js";

describe("workspaceRef", () => {
  it("carries the id, which is the identity", () => {
    expect(workspaceRef("ws-1", "/home/u/w").id).toBe("ws-1");
  });

  it("carries the dir, which is display material", () => {
    expect(workspaceRef("ws-1", "/home/u/w").dir).toBe("/home/u/w");
  });

  it("echoes an opaque id verbatim rather than parsing it", () => {
    const id = "ws/2026-08-29::odd~chars";
    expect(workspaceRef(id, "/w").id).toBe(id);
  });

  it("builds a real generated message, not an object literal", () => {
    expect(workspaceRef("ws-1", "/w").$typeName).toBe("workspace.v1.WorkspaceRef");
  });

  it("builds an equal ref for equal inputs, so nothing depends on identity", () => {
    expect(workspaceRef("ws-1", "/w")).toEqual(workspaceRef("ws-1", "/w"));
  });
});
