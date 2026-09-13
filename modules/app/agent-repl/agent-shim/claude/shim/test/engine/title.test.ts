/**
 * The incremental scan for the vendor's title.
 *
 * WHAT THIS GUARDS: that the title is found without re-reading a transcript
 * that grows all session, that it is stated once per CHANGE rather than once
 * per look, and that a half-written final line is read on the next pass rather
 * than swallowed. The failure modes being excluded are a per-turn cost that
 * grows with the conversation, a topbar republished on every turn with a title
 * that did not change, and a record lost across a read boundary.
 */
import { appendFileSync, mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { TranscriptTitleTail } from "../../src/engine/title.js";

/** A scratch transcript path, written or not. */
function scratch(): string {
  return path.join(mkdtempSync(path.join(os.tmpdir(), "shim-title-")), "session.jsonl");
}

/** One transcript line, terminated as the vendor terminates it. */
function line(record: Record<string, unknown>): string {
  return `${JSON.stringify(record)}\n`;
}

describe("the transcript title tail", () => {
  it("states the title the transcript already holds", () => {
    const file = scratch();
    writeFileSync(file, line({ type: "ai-title", aiTitle: "Add SPC j keybinding support" }), "utf8");

    expect(new TranscriptTitleTail(file).read()).toBe("Add SPC j keybinding support");
  });

  it("states nothing when the transcript does not exist yet", () => {
    expect(new TranscriptTitleTail(scratch()).read()).toBeUndefined();
  });

  it("states nothing on a second look at an unchanged transcript", () => {
    const file = scratch();
    writeFileSync(file, line({ type: "ai-title", aiTitle: "the one title" }), "utf8");
    const tail = new TranscriptTitleTail(file);
    tail.read();

    expect(tail.read()).toBeUndefined();
  });

  it("states nothing when the vendor restates the SAME title", () => {
    const file = scratch();
    writeFileSync(file, line({ type: "ai-title", aiTitle: "the one title" }), "utf8");
    const tail = new TranscriptTitleTail(file);
    tail.read();
    appendFileSync(file, line({ type: "ai-title", aiTitle: "the one title" }), "utf8");

    expect(tail.read()).toBeUndefined();
  });

  it("states a title the vendor changed its mind about", () => {
    const file = scratch();
    writeFileSync(file, line({ type: "ai-title", aiTitle: "first guess" }), "utf8");
    const tail = new TranscriptTitleTail(file);
    tail.read();
    appendFileSync(file, line({ type: "ai-title", aiTitle: "what it turned out to be" }), "utf8");

    expect(tail.read()).toBe("what it turned out to be");
  });

  it("leaves a half-written final line for the next read", () => {
    const file = scratch();
    writeFileSync(file, line({ type: "user", uuid: "u1" }), "utf8");
    const tail = new TranscriptTitleTail(file);
    tail.read();
    const whole = line({ type: "ai-title", aiTitle: "arrived in two writes" });
    const cut = whole.length - 5;
    appendFileSync(file, whole.slice(0, cut), "utf8");

    expect(tail.read()).toBeUndefined();

    appendFileSync(file, whole.slice(cut), "utf8");
    expect(tail.read()).toBe("arrived in two writes");
  });

  it("rereads a transcript that got SHORTER, because it is a different file now", () => {
    const file = scratch();
    writeFileSync(
      file,
      line({ type: "user", uuid: "u1" }) + line({ type: "ai-title", aiTitle: "before the rewrite" }),
      "utf8",
    );
    const tail = new TranscriptTitleTail(file);
    tail.read();
    writeFileSync(file, line({ type: "ai-title", aiTitle: "after" }), "utf8");

    expect(tail.read()).toBe("after");
  });

  it("reads only what was appended, so an earlier title is not restated", () => {
    const file = scratch();
    writeFileSync(file, line({ type: "ai-title", aiTitle: "the title" }), "utf8");
    const tail = new TranscriptTitleTail(file);
    tail.read();
    appendFileSync(file, line({ type: "assistant", uuid: "a1" }), "utf8");

    expect(tail.read()).toBeUndefined();
  });
});
