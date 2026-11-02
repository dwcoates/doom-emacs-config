/**
 * The shim's record sinks: a wait settles on its record, and once the records
 * have ended it settles on that end instead of hanging to the test budget.
 */
import { appendFileSync, mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { PassThrough } from "node:stream";
import { afterEach, describe, expect, it } from "vitest";
import { LogTail, operationIs, StreamRecords, type RecordSink } from "./log.js";

const line = (operation: string): string =>
  `${JSON.stringify({ level: "info", operation, message: "m", context: {} })}\n`;

const opened: RecordSink[] = [];
afterEach(() => {
  for (const sink of opened.splice(0)) sink.close();
});

/** A file sink over a fresh log, and the append that writes to it. */
function fileSink(): { sink: LogTail; write: (operation: string) => void } {
  const path = join(mkdtempSync(join(tmpdir(), "shim-log-")), "shim.log");
  writeFileSync(path, "");
  const sink = LogTail.open(path);
  opened.push(sink);
  return { sink, write: (operation) => appendFileSync(path, line(operation)) };
}

describe("LogTail.endOfRecords", () => {
  it("rejects a wait still pending, naming why the records ended", async () => {
    // Arrange
    const { sink } = fileSink();
    const pending = sink.record(operationIs("shim.never"));

    // Act
    sink.endOfRecords("the shim exited (code 1)");

    // Assert
    await expect(pending).rejects.toThrow("no awaited record can arrive any more: the shim exited (code 1)");
  });

  it("settles a wait on a record the writer left before it exited", async () => {
    // Arrange
    const { sink, write } = fileSink();
    write("shim.last");

    // Act
    sink.endOfRecords("the shim exited");

    // Assert
    await expect(sink.record(operationIs("shim.last"))).resolves.toMatchObject({ operation: "shim.last" });
  });

  it("rejects a later wait no record matched", async () => {
    // Arrange
    const { sink } = fileSink();
    sink.endOfRecords("the shim exited");

    // Act
    const late = sink.record(operationIs("shim.never"));

    // Assert
    await expect(late).rejects.toThrow("the shim exited");
  });
});

describe("StreamRecords", () => {
  it("ends its records when its pipe ends, rejecting a pending wait", async () => {
    // Arrange
    const pipe = new PassThrough();
    const sink = new StreamRecords(pipe);
    const pending = sink.record(operationIs("shim.never"));

    // Act
    pipe.end(line("shim.other"));

    // Assert
    await expect(pending).rejects.toThrow("the shim's log pipe ended");
    expect(sink.records().map((r) => r.operation)).toEqual(["shim.other"]);
  });
});
