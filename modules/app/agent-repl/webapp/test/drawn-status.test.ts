import { readFileSync, readdirSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createDrawnStatusLog } from "../src/drawn-status.js";
import { captureLogRecords, type LogCapture } from "./log-capture.js";

const OPERATION = "test.drawn-status";

/** The drawn-status records a capture holds, in order. */
async function drawn(capture: LogCapture) {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent.filter((record) => record.operation === OPERATION);
}

describe("createDrawnStatusLog", () => {
  it("records the first status a key draws at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    const statuses = createDrawnStatusLog("footer", OPERATION);
    // Act
    statuses.note("w1", { arm: "working", substatus: "thinking", source: "daemon" });
    // Assert
    const records = await drawn(capture);
    expect(records.map((r) => [r.level.case, r.message, r.context])).toEqual([
      [
        "info",
        "the footer now draws working",
        expect.objectContaining({ key: "w1", arm: "working", substatus: "thinking", source: "daemon" }),
      ],
    ]);
  });

  it("records nothing when a key redraws the same status", async () => {
    // Arrange
    const capture = captureLogRecords();
    const statuses = createDrawnStatusLog("footer", OPERATION);
    statuses.note("w1", { arm: "working", source: "daemon" });
    // Act
    statuses.note("w1", { arm: "working", source: "daemon" });
    // Assert
    expect(await drawn(capture)).toHaveLength(1);
  });

  it("records a changed arm with the previous one beside it", async () => {
    // Arrange
    const capture = captureLogRecords();
    const statuses = createDrawnStatusLog("footer", OPERATION);
    statuses.note("w1", { arm: "working", substatus: "thinking", source: "daemon" });
    // Act
    statuses.note("w1", { arm: "agentReplFault", substatus: "severed", source: "daemon" });
    // Assert
    const records = await drawn(capture);
    expect(records[1].context).toEqual(
      expect.objectContaining({
        arm: "agentReplFault",
        substatus: "severed",
        previous_arm: "working",
        previous_substatus: "thinking",
        previous_source: "daemon",
      }),
    );
  });

  it("records a changed substatus under the same arm", async () => {
    // Arrange
    const capture = captureLogRecords();
    const statuses = createDrawnStatusLog("footer", OPERATION);
    statuses.note("w1", { arm: "working", substatus: "submitting", source: "daemon" });
    // Act
    statuses.note("w1", { arm: "working", substatus: "thinking", source: "daemon" });
    // Assert
    expect(await drawn(capture)).toHaveLength(2);
  });

  it("records a change of source alone", async () => {
    // Arrange: the same words, but now the client's verdict rather than the daemon's.
    const capture = captureLogRecords();
    const statuses = createDrawnStatusLog("footer", OPERATION);
    statuses.note("w1", { arm: "disconnected", source: "daemon" });
    // Act
    statuses.note("w1", { arm: "disconnected", source: "client_verdict" });
    // Assert
    expect(await drawn(capture)).toHaveLength(2);
  });

  it("keeps each key's memory apart", async () => {
    // Arrange
    const capture = captureLogRecords();
    const statuses = createDrawnStatusLog("sidebar row", OPERATION);
    statuses.note("w1", { arm: "thinking", source: "daemon" });
    // Act
    statuses.note("w2", { arm: "thinking", source: "daemon" });
    // Assert
    expect((await drawn(capture)).map((r) => (r.context as { key: string }).key)).toEqual(["w1", "w2"]);
  });
});

describe("the drawn-status records' one shape", () => {
  it("is written only through createDrawnStatusLog", () => {
    // Arrange: every source file that names a drawn-status operation.
    const root = join(__dirname, "..", "src");
    const files: string[] = [];
    const walk = (dir: string): void => {
      for (const entry of readdirSync(dir, { withFileTypes: true })) {
        const path = join(dir, entry.name);
        if (entry.isDirectory()) walk(path);
        else if (entry.name.endsWith(".ts")) files.push(path);
      }
    };
    walk(root);
    // Act
    const handRolled = files.filter((file) => {
      const source = readFileSync(file, "utf8");
      return /"[a-z-]+\.drawn-status"/.test(source) && !/createDrawnStatusLog\([^)]*"[a-z-]+\.drawn-status"\)/.test(source);
    });
    // Assert
    expect(handRolled).toEqual([]);
  });
});
