import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it, vi } from "vitest";
import {
  CloseWorkspaceErrorSchema,
  CloseWorkspaceTransferringAwaySchema,
  CloseWorkspaceWorkspaceRefMismatchSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_workspace_pb";
import { OpenInEditorErrorSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import { oneofArms } from "../arms.js";
import { isMalformedView } from "../../src/rpc/malformed.js";
import { log } from "../../src/log.js";
import { CROSS_CUTTING_REFUSAL_ARMS, noteRefusedArm, refusalSentence } from "../../src/rpc/refusal.js";

describe("CROSS_CUTTING_REFUSAL_ARMS", () => {
  it("names exactly the four arms every per-workspace rpc shares", () => {
    expect([...CROSS_CUTTING_REFUSAL_ARMS].sort()).toEqual([
      "notYetAdopted",
      "transferringAway",
      "unknownWorkspace",
      "workspaceRefMismatch",
    ]);
  });

  it("is a subset of a real endpoint's arms, so a renamed arm fails here", () => {
    const arms = oneofArms(CloseWorkspaceErrorSchema, "cause");
    for (const arm of CROSS_CUTTING_REFUSAL_ARMS) expect(arms).toContain(arm);
  });

  it("is answered for on every endpoint that carries the four", () => {
    for (const arm of oneofArms(OpenInEditorErrorSchema, "cause")) {
      const said = refusalSentence("OpenInEditor", { case: arm, value: { registryDir: "d", address: "a" } });
      expect(CROSS_CUTTING_REFUSAL_ARMS.includes(arm)).toBe(said !== undefined);
    }
  });
});

describe("refusalSentence", () => {
  it("says the daemon does not know the workspace", () => {
    expect(refusalSentence("CloseWorkspace", { case: "unknownWorkspace", value: {} })).toBe(
      "the daemon does not know this workspace",
    );
  });

  it("names the registry's directory on a ref mismatch", () => {
    const value = create(CloseWorkspaceWorkspaceRefMismatchSchema, { registryDir: "/w/other" });
    expect(refusalSentence("CloseWorkspace", { case: "workspaceRefMismatch", value })).toBe(
      "this workspace's directory disagrees with the registry's: /w/other",
    );
  });

  it("names the successor daemon's address on a transfer", () => {
    const value = create(CloseWorkspaceTransferringAwaySchema, { address: "127.0.0.1:7777" });
    expect(refusalSentence("CloseWorkspace", { case: "transferringAway", value })).toBe(
      "this workspace moved to another daemon at 127.0.0.1:7777",
    );
  });

  it("says adoption is unfinished", () => {
    expect(refusalSentence("CloseWorkspace", { case: "notYetAdopted", value: {} })).toBe(
      "the daemon has not finished adopting this workspace yet",
    );
  });

  it("answers undefined for a per-rpc arm, leaving the wording to the site", () => {
    expect(refusalSentence("CloseWorkspace", { case: "blocked", value: {} })).toBeUndefined();
  });

  it("answers undefined for an arm no build knows", () => {
    expect(refusalSentence("CloseWorkspace", { case: "somethingNewer", value: {} })).toBeUndefined();
  });

  it("refuses a ref mismatch carrying no directory", () => {
    expect(() => refusalSentence("CloseWorkspace", { case: "workspaceRefMismatch", value: {} })).toThrow(
      /expected a string/,
    );
  });

  it("refuses a transfer carrying no address", () => {
    expect(() => refusalSentence("CloseWorkspace", { case: "transferringAway", value: {} })).toThrow(
      /expected a string/,
    );
  });

  it("raises the refusal as a MalformedView naming the endpoint's field", () => {
    try {
      refusalSentence("CloseWorkspace", { case: "transferringAway", value: undefined });
      expect.unreachable("a missing address must refuse");
    } catch (err) {
      expect(isMalformedView(err)).toBe(true);
      expect((err as { path: string }).path).toBe(
        "CloseWorkspaceError.cause.transferringAway.address",
      );
    }
  });
});

describe("noteRefusedArm", () => {
  it.each([
    ["a success", { case: "success", value: {} }, null],
    ["a named cause", { case: "error", value: { cause: { case: "noEditor", value: {} } } }, "noEditor"],
    ["an unset cause", { case: "error", value: { cause: { case: undefined } } }, "unset"],
  ])("answers the arm of %s", (_name, result, want) => {
    // Act / Assert
    expect(noteRefusedArm("RequestNewThread", "test.refused", result)).toBe(want);
  });

  it("records a refusal at INFO under the site's operation", () => {
    // Arrange
    const info = vi.spyOn(log, "info");

    // Act
    noteRefusedArm("SelectWorkspace", "sidebar.app-refused", { case: "error", value: { cause: { case: "unknownWorkspace" } } });

    // Assert
    expect(info).toHaveBeenCalledWith("SelectWorkspace was refused: unknownWorkspace", {
      operation: "sidebar.app-refused",
      context: { rpc: "SelectWorkspace", arm: "unknownWorkspace" },
    });
    info.mockRestore();
  });

  it("records nothing for a success", () => {
    // Arrange
    const info = vi.spyOn(log, "info");

    // Act
    noteRefusedArm("SelectChatText", "chat.gesture-refused", { case: "success", value: {} });

    // Assert
    expect(info).not.toHaveBeenCalled();
    info.mockRestore();
  });

  it.each([
    ["src/chat/chat.tsx", "chat.gesture-refused"],
    ["src/sidebar/threads.ts", "sidebar.app-refused"],
  ])("is how %s records a refusal the next view answers, through sendGesture", (file, operation) => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../..", file), "utf8");

    // Assert: every gesture goes through rpc/gesture.ts's sendGesture, which
    // is noteRefusedArm's one caller there (test/rpc/gesture.test.ts).
    expect(source).toContain(`const GESTURE_REFUSED = "${operation}";`);
    expect(source).toContain("sendGesture(");
    expect(source).not.toMatch(/was refused: \$\{cause\}/);
  });
});
