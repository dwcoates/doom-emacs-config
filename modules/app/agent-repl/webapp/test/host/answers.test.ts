// How each of Emacs's web views answers a call. See src/host/answers.ts.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

import { createCallAnswers } from "../../src/host/answers.js";
import { codeOf } from "../source-text.js";

/** An answer, parsed. */
function parsed(answer: string): Record<string, unknown> {
  return JSON.parse(answer) as Record<string, unknown>;
}

describe("createCallAnswers", () => {
  it("answers the call, its workspace and its outcome", () => {
    // Act
    const answer = parsed(createCallAnswers(5).answer("show", "ws-a", "shown"));

    // Assert
    expect(answer).toEqual({ op: "show", id: "ws-a", outcome: "shown", notes: [] });
  });

  it("carries the fields the call adds", () => {
    // Act
    const answer = parsed(createCallAnswers(5).answer("setDaemon", null, "retargeted", { origin: "http://h:1" }));

    // Assert
    expect(answer["origin"]).toBe("http://h:1");
  });

  it("carries every note made since the last answer, at its level", () => {
    // Arrange
    const answers = createCallAnswers(5);
    answers.note("info", "cooled");
    answers.note("warn", "unreadable");

    // Act
    const answer = parsed(answers.answer("show", null, "shown"));

    // Assert
    expect(answer["notes"]).toEqual([
      { level: "info", text: "cooled" },
      { level: "warn", text: "unreadable" },
    ]);
  });

  it("hands a note to one answer only", () => {
    // Arrange
    const answers = createCallAnswers(5);
    answers.note("warn", "once");
    answers.answer("show", null, "shown");

    // Act
    const answer = parsed(answers.answer("show", null, "shown"));

    // Assert
    expect(answer["notes"]).toEqual([]);
  });

  it("keeps only the newest notes past its limit", () => {
    // Arrange
    const answers = createCallAnswers(2);
    for (const text of ["a", "b", "c"]) answers.note("info", text);

    // Act
    const answer = parsed(answers.answer("show", null, "shown"));

    // Assert
    expect((answer["notes"] as { text: string }[]).map((n) => n.text)).toEqual(["b", "c"]);
  });
});

describe("the documents Emacs calls", () => {
  it.each(["host/host.ts", "host/sidebar-page.ts"])("%s answers through createCallAnswers, never by hand", (path) => {
    // Arrange
    const code = codeOf(readFileSync(join(__dirname, "..", "..", "src", path), "utf8"));

    // Act
    const shape = { shared: code.includes("createCallAnswers("), handRolled: /JSON\.stringify\(\{\s*op\b/.test(code) };

    // Assert
    expect(shape).toEqual({ shared: true, handRolled: false });
  });
});
