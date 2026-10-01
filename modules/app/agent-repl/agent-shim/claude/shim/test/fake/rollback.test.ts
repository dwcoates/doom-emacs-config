/**
 * fake/rollback.ts — the mocked vendor's truncating-resume boot and its file
 * checkpointing, judged off the transcript as the CLI judges them.
 */
import { mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { fakeRewindFiles, judgeTruncatingResume } from "../../src/fake/rollback.js";
import { RESUME_DROPS_TURN_REFUSAL_PREFIX } from "../../src/engine/rollback.js";

function transcript(lines: Record<string, unknown>[]): string {
  const file = path.join(mkdtempSync(path.join(os.tmpdir(), "shim-fake-rollback-")), "session.jsonl");
  writeFileSync(file, `${lines.map((line) => JSON.stringify(line)).join("\n")}\n`);
  return file;
}

const user = (uuid: string, parentUuid: string | null, content: unknown = `prompt ${uuid}`): Record<string, unknown> => ({
  type: "user",
  uuid,
  parentUuid,
  message: { role: "user", content },
});

const assistant = (uuid: string, parentUuid: string, content: unknown[] = [{ type: "text", text: "ok" }]) => ({
  type: "assistant",
  uuid,
  parentUuid,
  message: { role: "assistant", content },
});

const edit = (file: string): Record<string, unknown> => ({
  type: "tool_use",
  id: `toolu_${file}`,
  name: "Edit",
  input: { file_path: file, old_string: "a", new_string: "b" },
});

describe("judgeTruncatingResume", () => {
  it("boots a resume at a fork point the conversation holds", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0")]);

    // Act.
    const boot = judgeTruncatingResume(file, "a0", undefined);

    // Assert.
    expect(boot).toEqual({ kind: "booted" });
  });

  it("refuses a fork point the conversation does not hold, in the CLI's words", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0")]);

    // Act.
    const boot = judgeTruncatingResume(file, "a9", undefined);

    // Assert.
    expect(boot).toEqual({ kind: "refused", message: "No message found with message.uuid of: a9" });
  });

  it("boots a guarded cut whose discarded range is the dropped turn's own", () => {
    // Arrange.
    const file = transcript([
      user("p0", null),
      assistant("a0", "p0"),
      user("p1", "a0"),
      assistant("a1", "p1"),
      user("r1", "a1", [{ type: "tool_result", tool_use_id: "t", content: "x" }]),
      user("m1", "r1", "[Request interrupted by user]"),
    ]);

    // Act.
    const boot = judgeTruncatingResume(file, "a0", "p1");

    // Assert.
    expect(boot).toEqual({ kind: "booted" });
  });

  it("refuses a guarded cut that would discard another turn's prompt", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1"), user("p2", "a1")]);

    // Act.
    const boot = judgeTruncatingResume(file, "a0", "p1");

    // Assert.
    expect(boot.kind === "refused" ? boot.message.startsWith(RESUME_DROPS_TURN_REFUSAL_PREFIX) : false).toBe(true);
  });
});

describe("fakeRewindFiles", () => {
  it("answers canRewind false for a message the transcript holds no user record of", () => {
    // Arrange.
    const file = transcript([user("p0", null)]);

    // Act.
    const rewind = fakeRewindFiles(file, "p9");

    // Assert.
    expect(rewind.canRewind).toBe(false);
  });

  it("names the files the edit tools touched from the prompt on, and none before it", () => {
    // Arrange.
    const file = transcript([
      user("p0", null),
      assistant("a0", "p0", [edit("/ws/before.ts")]),
      user("p1", "a0"),
      assistant("a1", "p1", [edit("/ws/a.ts")]),
      user("p2", "a1"),
      assistant("a2", "p2", [edit("/ws/b.ts"), edit("/ws/a.ts")]),
    ]);

    // Act.
    const rewind = fakeRewindFiles(file, "p1");

    // Assert.
    expect(rewind).toEqual({ canRewind: true, filesChanged: ["/ws/a.ts", "/ws/b.ts"], insertions: 0, deletions: 0 });
  });
});
