// How each of Emacs's web views answers a call. See src/host/answers.ts.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { beforeEach, describe, expect, it } from "vitest";

import { createCallAnswers, runQueuedCalls } from "../../src/host/answers.js";
import { captureLogRecords, forwardedRecord, type LogCapture } from "../log-capture.js";
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

describe("runQueuedCalls", () => {
  let capture: LogCapture;

  beforeEach(() => {
    capture = captureLogRecords();
  });

  it("runs every queued call on the page, in order", () => {
    // Arrange
    const order: string[] = [];

    // Act
    runQueuedCalls(
      [(page: string) => order.push(`1:${page}`), (page: string) => order.push(`2:${page}`)],
      "the page",
    );

    // Assert
    expect(order).toEqual(["1:the page", "2:the page"]);
  });

  it("records each call's op, id and outcome", async () => {
    // Arrange
    const { answer } = createCallAnswers(1);

    // Act
    runQueuedCalls([() => answer("show", "ws-a", "shown", { previous: null })], null);

    // Assert
    expect((await forwardedRecord(capture, "webhost.boot-started")).context).toMatchObject({
      drained: 1,
      answers: [{ op: "show", id: "ws-a", outcome: "shown" }],
    });
  });

  it("records an answer that is not a call answer as it is", async () => {
    // Act
    runQueuedCalls([() => 3], null);

    // Assert
    expect((await forwardedRecord(capture, "webhost.boot-started")).context).toMatchObject({ answers: [3] });
  });

  it("records a start with nothing queued", async () => {
    // Act
    runQueuedCalls(undefined, null);

    // Assert
    expect((await forwardedRecord(capture, "webhost.boot-started")).context).toMatchObject({ drained: 0, answers: [] });
  });

  it("names the call that threw, and runs none after it", () => {
    // Arrange
    let after = false;
    const calls = [
      () => undefined,
      () => {
        throw new TypeError("no such page");
      },
      () => {
        after = true;
      },
    ];

    // Act
    const act = () => runQueuedCalls(calls, null);

    // Assert
    expect(act).toThrow("queued call 2 of 3 threw: TypeError: no such page");
    expect(after).toBe(false);
  });

  it("throws when a string answer is not JSON, since every call answer is", () => {
    // Act
    const act = () => runQueuedCalls([() => "not json"], null);

    // Assert
    expect(act).toThrow("queued call 1 of 1 threw: SyntaxError");
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

  it.each(["host/main.ts", "host/sidebar-page.ts"])("%s runs its queued calls through runQueuedCalls, never by hand", (path) => {
    // Arrange
    const code = codeOf(readFileSync(join(__dirname, "..", "..", "src", path), "utf8"));

    // Act
    const shape = { shared: code.includes("runQueuedCalls("), handRolled: /for \(const \w+ of pending\)/.test(code) };

    // Assert
    expect(shape).toEqual({ shared: true, handRolled: false });
  });
});
