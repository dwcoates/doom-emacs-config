import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import {
  WorkspaceHasOpenChildrenSchema,
  WorkspaceParentNotOpenSchema,
} from "../../../proto/gen/ts/agentrepl/v1/family_pb";
import {
  hasOpenChildrenSentence,
  mergeHasOpenChildrenSentence,
  parentNotOpenSentence,
} from "../../src/rpc/family-refusal.js";
import { MalformedView } from "../../src/rpc/malformed.js";

const PARENT = { id: "p1", dir: "/w/doom/parent-ws" };

function openChildren(children: Array<{ id: string; dir: string }>) {
  return create(WorkspaceHasOpenChildrenSchema, { parent: PARENT, children });
}

function parentNotOpen(state: MessageInitShape<typeof WorkspaceParentNotOpenSchema>["state"]) {
  return create(WorkspaceParentNotOpenSchema, { parent: PARENT, state });
}

describe("hasOpenChildrenSentence", () => {
  it.each([
    {
      name: "one child is named and closing it is asked for",
      children: [{ id: "c1", dir: "/w/doom/child-a" }],
      want: "close refused: child-a is still open under this workspace; close it first",
    },
    {
      name: "several children are all named",
      children: [
        { id: "c1", dir: "/w/doom/child-a" },
        { id: "c2", dir: "/w/doom/child-b" },
      ],
      want: "close refused: child-a, child-b are still open under this workspace; close them first",
    },
    {
      name: "a child with no directory is named by its id",
      children: [{ id: "c1", dir: "" }],
      want: "close refused: c1 is still open under this workspace; close it first",
    },
  ])("$name", ({ children, want }) => {
    // Arrange
    const u = openChildren(children);
    // Act
    const said = hasOpenChildrenSentence(u, "CloseWorkspaceError.has_open_children", "close");
    // Assert
    expect(said).toBe(want);
  });

  it("refuses a refusal that names no child", () => {
    // Arrange
    const u = openChildren([]);
    // Act / Assert
    expect(() => hasOpenChildrenSentence(u, "CloseWorkspaceError.has_open_children", "close")).toThrow(MalformedView);
  });
});

describe("mergeHasOpenChildrenSentence", () => {
  it("names the parent and its children and asks for them to be resolved and closed before a one-shot merge", () => {
    // Arrange
    const u = openChildren([{ id: "c1", dir: "/w/doom/child-a" }]);
    // Act
    const said = mergeHasOpenChildrenSentence(u, "MergeWorkspaceError.has_open_children");
    // Assert
    expect(said).toBe(
      "parent-ws still has open child workspaces (child-a); resolve and close all of them before a one-shot merge",
    );
  });

  it("refuses a refusal with no parent", () => {
    // Arrange
    const u = create(WorkspaceHasOpenChildrenSchema, { children: [{ id: "c1", dir: "/w/c" }] });
    // Act / Assert
    expect(() => mergeHasOpenChildrenSentence(u, "MergeWorkspaceError.has_open_children")).toThrow(MalformedView);
  });

  it("refuses a refusal that names no child", () => {
    // Arrange
    const u = openChildren([]);
    // Act / Assert
    expect(() => mergeHasOpenChildrenSentence(u, "MergeWorkspaceError.has_open_children")).toThrow(MalformedView);
  });
});

describe("parentNotOpenSentence", () => {
  it.each([
    {
      name: "a hidden parent is to be shown first",
      state: { case: "hidden", value: {} } as const,
      want: "reopen refused: its parent parent-ws is hidden; show the parent first",
    },
    {
      name: "a closed parent is to be reopened first",
      state: { case: "closed", value: {} } as const,
      want: "reopen refused: its parent parent-ws is closed; reopen the parent first",
    },
  ])("$name", ({ state, want }) => {
    // Arrange
    const u = parentNotOpen(state);
    // Act
    const said = parentNotOpenSentence(u, "OpenWorkspaceError.parent_not_open", "reopen");
    // Assert
    expect(said).toBe(want);
  });

  it("refuses a refusal whose state is unset", () => {
    // Arrange
    const u = parentNotOpen(undefined);
    // Act / Assert
    expect(() => parentNotOpenSentence(u, "OpenWorkspaceError.parent_not_open", "reopen")).toThrow(MalformedView);
  });

  it("refuses a refusal with no parent", () => {
    // Arrange
    const u = create(WorkspaceParentNotOpenSchema, { state: { case: "hidden", value: {} } });
    // Act / Assert
    expect(() => parentNotOpenSentence(u, "OpenWorkspaceError.parent_not_open", "reopen")).toThrow(MalformedView);
  });
});
