/**
 * CLIENT LOG — the webapp's diagnostics, on the wire.
 *
 * The page has no console anybody reads: it runs inside an xwidget, so a
 * warning that stayed in the browser console would leave no evidence anywhere.
 * `ClientLog` is the daemon's ear, and `src/log.ts` is the ONLY way the app
 * speaks into it — so what this file asserts is that a record emitted through
 * the canonical API arrives as a `ClientLogRecord` with the matching level arm,
 * the call site's operation, and the page's own workspace echoed.
 *
 * The sink is production's: the harness wires `ClientLog` the way main.ts does
 * (`clientLogSink`, one call per record, deliberately not through `callUnary`),
 * behind the same throttle — which is why a warn is asserted after its two
 * second window and an error is asserted immediately.
 *
 * Most cases install that sink AFTER the boot (`installClientLogSink`), so the
 * only records on the wire are the ones the case emits. The two cases that are
 * about the boot's own diagnostics boot with it standing (`clientLog: true`).
 */
import { afterEach, describe, expect, it } from "vitest";

import { ClientLogRecordSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";

import { startHarness, type Harness } from "./harness";
import { WORKSPACE_ID, armsOf } from "./fixtures";
import { log } from "../../src/log";
import type { ClientLogRecord } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";

let harness: Harness;

afterEach(async () => {
  await harness?.stop();
});

/** One `ClientLog` request, as the fake recorded it. */
interface LoggedCall {
  workspace?: { id: string; dir: string };
  record?: ClientLogRecord;
}

/** The records the daemon has been handed, oldest first. */
const recorded = (): LoggedCall[] => harness.fake.calls<LoggedCall>("clientLog");

/** Boot quiet, then install production's log sink for the record under test. */
async function withLogSink(): Promise<void> {
  // BOOTED QUIET, SINK INSTALLED AFTER. The wiring is main.ts's own either
  // way (`installClientLogSink` is exactly what `clientLog: true` calls); what
  // changes is that the boot's own diagnostics never reach the wire.
  //
  // Each forwarded record is its own unary round trip to the fake by design
  // (`ClientLogRecord` carries one record, so there is no batch arm). The boot
  // emits ~56, which crosses the throttle's `maxBatch` of 50 and so flushes
  // fifty of them as fifty real socket trips DURING the boot, with the
  // remainder draining on the first window: measured, a `clientLog: true` boot
  // costs ~283ms against a quiet one's ~105ms, and every one of those trips is
  // real I/O that stretches under parallel load. That was ~180ms of each
  // case's 900ms budget spent on records the case does not assert about, which
  // is why this group straddled the bound.
  //
  // The records under test are the ones a case emits after this point, so the
  // boot's are not merely cleared, they are never sent.
  harness = await startHarness();
  harness.installClientLogSink();
  harness.fake.clearCalls();
}

/** The throttle releases a non-error record on its two second window. */
const flush = async (): Promise<void> => harness.tick(2_000);

describe("the level arms", () => {
  it("names every level arm the record declares", () => {
    // Assert: the four the logger's API offers ARE the four on the wire.
    expect(armsOf(ClientLogRecordSchema, "level").sort()).toEqual([
      "debug",
      "error",
      "info",
      "warn",
    ]);
  });
});

describe("a warning", () => {
  it("reaches the daemon", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("warn", "the view arrived thin", { operation: "harness.warn-case" });
    await flush();
    // Assert
    expect(recorded().length).toBeGreaterThan(0);
  });

  it("carries the warn level arm", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("warn", "the view arrived thin", { operation: "harness.warn-case" });
    await flush();
    // Assert
    expect(recorded().at(-1)?.record?.level.case).toBe("warn");
  });

  it("carries the call site's operation", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("warn", "the view arrived thin", { operation: "harness.warn-case" });
    await flush();
    // Assert
    expect(recorded().at(-1)?.record?.operation).toBe("harness.warn-case");
  });

  it("carries the message verbatim", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("warn", "the view arrived thin", { operation: "harness.warn-case" });
    await flush();
    // Assert
    expect(recorded().at(-1)?.record?.message).toBe("the view arrived thin");
  });

  it("echoes the page's own workspace", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("warn", "the view arrived thin", { operation: "harness.warn-case" });
    await flush();
    // Assert
    expect(recorded().at(-1)?.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("carries the call site's evidence in the record's context", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("warn", "the view arrived thin", {
      operation: "harness.warn-case",
      context: { rows: 3 },
    });
    await flush();
    // Assert
    // The record's context is the routing identity plus the call site's own
    // evidence, which travels under its own `context` key.
    expect(recorded().at(-1)?.record?.context).toMatchObject({ context: { rows: 3 } });
  });

  it("waits for the throttle's window rather than calling per record", async () => {
    // Arrange
    await withLogSink();
    // Act: nothing is flushed yet.
    log("warn", "the view arrived thin", { operation: "harness.warn-case" });
    await harness.settle();
    // Assert
    expect(recorded()).toHaveLength(0);
  });
});

describe("an error", () => {
  it("carries the error level arm", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("error", "the stream died", { operation: "harness.error-case" });
    await harness.settle();
    // Assert
    expect(recorded().at(-1)?.record?.level.case).toBe("error");
  });

  it("does not wait for the throttle's window", async () => {
    // Arrange: an error is the record someone will go looking for.
    await withLogSink();
    // Act
    log("error", "the stream died", { operation: "harness.error-case" });
    await harness.settle();
    // Assert
    expect(recorded().length).toBeGreaterThan(0);
  });

  it("carries its own operation", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("error", "the stream died", { operation: "harness.error-case" });
    await harness.settle();
    // Assert
    expect(recorded().at(-1)?.record?.operation).toBe("harness.error-case");
  });

  it("echoes the page's own workspace", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("error", "the stream died", { operation: "harness.error-case" });
    await harness.settle();
    // Assert
    expect(recorded().at(-1)?.workspace?.id).toBe(WORKSPACE_ID);
  });
});

describe("an info record", () => {
  it("carries the info level arm", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("info", "the reader opened a bubble", { operation: "harness.info-case" });
    await flush();
    // Assert
    expect(recorded().at(-1)?.record?.level.case).toBe("info");
  });
});

describe("a debug record", () => {
  it("carries the debug level arm", async () => {
    // Arrange
    await withLogSink();
    // Act
    log("debug", "drawing a row", { operation: "harness.debug-case" });
    await flush();
    // Assert
    expect(recorded().at(-1)?.record?.level.case).toBe("debug");
  });
});

describe("the app's own diagnostics", () => {
  it("reach the daemon without a test emitting anything", async () => {
    // Arrange: the boot itself logs through the same sink main.ts installs.
    harness = await startHarness({ clientLog: true });
    // Act
    await harness.tick(2_000);
    // Assert
    expect(recorded().length).toBeGreaterThan(0);
  });

  it("echo the workspace the page was addressed to", async () => {
    // Arrange
    harness = await startHarness({ clientLog: true });
    // Act
    await harness.tick(2_000);
    // Assert
    expect(recorded().at(-1)?.workspace?.dir).toBe(harness.ctx.workspace.dir);
  });
});
