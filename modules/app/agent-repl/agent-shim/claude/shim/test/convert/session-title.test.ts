/**
 * The vendor's `ai-title` line, decoded.
 *
 * WHAT THIS GUARDS: that the one transcript line carrying the conversation's
 * own summary is read exactly as the vendor writes it, and that nothing else
 * is ever mistaken for it. The failure modes being excluded are a strip that
 * draws a blank title, a strip that draws a title the vendor never stated, and
 * a torn final line costing a turn its title.
 */
import { describe, expect, it } from "vitest";
import { aiTitleOf, lastAiTitle, sessionTitleUpdate } from "../../src/convert/session-title.js";

/** One transcript line, as the vendor writes it. */
function line(record: Record<string, unknown>): string {
  return JSON.stringify(record);
}

describe("one ai-title line", () => {
  it("states the title the vendor wrote", () => {
    const raw = line({ type: "ai-title", aiTitle: "Add SPC j keybinding support", sessionId: "s1" });

    expect(aiTitleOf(raw)).toBe("Add SPC j keybinding support");
  });

  it("states nothing for a line of another type", () => {
    const raw = line({ type: "permission-mode", mode: "plan", sessionId: "s1" });

    expect(aiTitleOf(raw)).toBeUndefined();
  });

  it("states nothing when the title is not a string", () => {
    const raw = line({ type: "ai-title", aiTitle: 7, sessionId: "s1" });

    expect(aiTitleOf(raw)).toBeUndefined();
  });

  it("states nothing for a blank title, which is worse than the name it would replace", () => {
    const raw = line({ type: "ai-title", aiTitle: "   ", sessionId: "s1" });

    expect(aiTitleOf(raw)).toBeUndefined();
  });

  it("states nothing for a torn line rather than throwing", () => {
    const raw = '{"type":"ai-title","aiTitl';

    expect(aiTitleOf(raw)).toBeUndefined();
  });

  it("states nothing for a blank line", () => {
    expect(aiTitleOf("   ")).toBeUndefined();
  });
});

describe("a chunk of transcript lines", () => {
  it("states the LAST title, because the vendor restates it as the work moves on", () => {
    const chunk = [
      line({ type: "ai-title", aiTitle: "first guess" }),
      line({ type: "user", uuid: "u1" }),
      line({ type: "ai-title", aiTitle: "what it turned out to be" }),
    ].join("\n");

    expect(lastAiTitle(chunk)).toBe("what it turned out to be");
  });

  it("states nothing when no line in it is a title", () => {
    const chunk = [line({ type: "user", uuid: "u1" }), line({ type: "assistant", uuid: "a1" })].join("\n");

    expect(lastAiTitle(chunk)).toBeUndefined();
  });
});

describe("the session fact", () => {
  it("carries the title verbatim on the title arm", () => {
    const update = sessionTitleUpdate("Add SPC j keybinding support");

    expect(update.update.case).toBe("title");
    expect(update.update.case === "title" ? update.update.value.text : "").toBe(
      "Add SPC j keybinding support",
    );
  });
});
