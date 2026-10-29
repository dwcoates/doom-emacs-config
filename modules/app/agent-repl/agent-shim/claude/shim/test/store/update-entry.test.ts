/**
 * store/update-entry.ts — the one shape of a row that carries an AgentUpdate.
 */
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import { updateEntry, type UpdateEntrySpec } from "../../src/store/update-entry.js";

const SRC = path.join(import.meta.dirname, "../../src");

function spec(overrides: Partial<UpdateEntrySpec> = {}): UpdateEntrySpec {
  return {
    agentId: create(conversationv1.AgentIdSchema, { value: "agent-1" }),
    upsertKey: "terminal:agent-1:cut-1",
    source: { vendorUuid: "cut-1", discriminator: "agent_frame.update.context_cut.cleared" },
    keepalive: false,
    turn: create(conversationv1.TurnIdSchema, { value: "turn-1" }),
    update: { case: "contextCut", value: create(conversationv1.ContextCutSchema, {}) },
    ...overrides,
  };
}

/** Every source file under DIR. */
function sourcesUnder(dir: string): string[] {
  return readdirSync(dir).flatMap((name) => {
    const full = path.join(dir, name);
    if (statSync(full).isDirectory()) return sourcesUnder(full);
    return full.endsWith(".ts") ? [full] : [];
  });
}

describe("updateEntry", () => {
  it("carries the key, the source, the keep-alive flag and the turn as stated", () => {
    // Arrange, Act
    const entry = updateEntry(spec());
    // Assert
    expect([entry.upsertKey, entry.source, entry.keepalive, entry.turn?.value]).toEqual([
      "terminal:agent-1:cut-1",
      { vendorUuid: "cut-1", discriminator: "agent_frame.update.context_cut.cleared" },
      false,
      "turn-1",
    ]);
  });

  it("files the row and its frame in the same agent's book", () => {
    // Arrange, Act
    const entry = updateEntry(spec());
    // Assert
    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    expect([entry.agentId?.value, frame?.agentId?.value]).toEqual(["agent-1", "agent-1"]);
  });

  it("carries the update as the frame's update arm", () => {
    // Arrange
    const revision = create(conversationv1.AgentResponseRevisionSchema, {
      prose: create(conversationv1.AgentResponseProseSchema, { markdown: "revised" }),
    });
    // Act
    const entry = updateEntry(spec({ update: { case: "responseRevision", value: revision } }));
    // Assert
    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const update = frame?.result.case === "update" ? frame.result.value.update : undefined;
    expect(update?.case === "responseRevision" ? update.value.prose?.markdown : "").toBe("revised");
  });

  it("states no turn when the row is no turn's", () => {
    // Arrange, Act
    const entry = updateEntry(spec({ turn: undefined }));
    // Assert
    expect(entry.turn).toBeUndefined();
  });
});

describe("the update row's writers", () => {
  it("build no update frame of their own outside the converter", () => {
    // Arrange
    const offenders: string[] = [];
    // Act
    for (const file of sourcesUnder(SRC)) {
      if (file.endsWith(path.join("convert", "entries.ts"))) continue;
      const code = readFileSync(file, "utf8");
      if (/case: "update",\s*value: create\(conversationv1\.AgentUpdateSchema/.test(code)) offenders.push(path.relative(SRC, file));
    }
    // Assert
    expect(offenders).toEqual([]);
  });
});
