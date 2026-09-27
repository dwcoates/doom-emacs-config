/**
 * The entrypoint's two log emitters.
 *
 * `reportFatal` is the reporter of last resort — it runs before argv is parsed
 * and after the durable sink has died — so what is pinned here is that it
 * classifies whatever it was handed and never lets its own logger's failure
 * replace the failure it was reporting.
 */
import { writeSync } from "node:fs";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { logRecordsSince } from "./log-records.js";
import { MAIN_FATAL_OPERATION, MAIN_LIFECYCLE_LOGGER, MAIN_LIFECYCLE_OPERATION, reportFatal } from "../src/fatal.js";

const mockedWriteSync = vi.mocked(writeSync);

/** The single record this test provoked. */
function onlyRecord(): Record<string, unknown> {
  const records = logRecordsSince(0);
  expect(records).toHaveLength(1);
  return records[0];
}

beforeEach(() => {
  mockedWriteSync.mockClear();
});

describe("MAIN_LIFECYCLE_LOGGER", () => {
  it("emits lifecycle records at the selected method's level", () => {
    // Arrange, Act.
    MAIN_LIFECYCLE_LOGGER.info({ outcome: "serving" }, "shim.v1 is being served");

    // Assert.
    expect(onlyRecord()).toMatchObject({
      level: "info",
      operation: MAIN_LIFECYCLE_OPERATION,
      message: "shim.v1 is being served",
      context: { outcome: "serving" },
    });
  });

  it("emits lifecycle errors through the error method", () => {
    // Arrange, Act.
    MAIN_LIFECYCLE_LOGGER.error({ outcome: "exit_before_serving" }, "no listener yet");

    // Assert.
    expect(onlyRecord()).toMatchObject({ level: "error" });
  });
});

describe("reportFatal classifies what it was handed", () => {
  it("names the error's own class as the cause type", () => {
    // Arrange.
    class SinkDied extends Error {
      override readonly name = "SinkDied";
    }

    // Act.
    reportFatal(new SinkDied("fd 3 is gone"));

    // Assert.
    expect(onlyRecord()).toMatchObject({
      operation: MAIN_FATAL_OPERATION,
      level: "error",
      context: { cause_type: "SinkDied", cause_class: "unrecoverable_entrypoint_failure" },
    });
  });

  it("falls back to \"Error\" for an error whose name was blanked", () => {
    // Arrange — a thrown Error can carry an empty name, and an empty
    // cause_type would tell a reader nothing at all.
    const err = new Error("nameless");
    err.name = "";

    // Act.
    reportFatal(err);

    // Assert.
    expect(onlyRecord()).toMatchObject({ context: { cause_type: "Error" } });
  });

  it("reports the typeof for a thrown value that is not an Error", () => {
    // Arrange, Act.
    reportFatal("the store socket vanished");

    // Assert.
    expect(onlyRecord()).toMatchObject({
      context: { cause_type: "string" },
      message: "fatal: the store socket vanished",
    });
  });

  it("prefers the stack over the message when the error carries one", () => {
    // Arrange.
    const err = new Error("boom");
    err.stack = "Error: boom\n    at startup";

    // Act.
    reportFatal(err);

    // Assert.
    expect(onlyRecord()["message"]).toBe("fatal: Error: boom\n    at startup");
  });

  it("falls back to the message when the error carries no stack", () => {
    // Arrange — a rejection built by a native binding can arrive stackless.
    const err = new Error("boom");
    delete (err as { stack?: string }).stack;

    // Act.
    reportFatal(err);

    // Assert.
    expect(onlyRecord()["message"]).toBe("fatal: boom");
  });
});

/**
 * The reporter of last resort must survive its OWN logger failing: it exists
 * precisely for the window in which the durable sink is the broken thing.
 */
describe("reportFatal when its logger itself fails", () => {
  afterEach(() => {
    vi.doUnmock("../src/log.js");
    vi.resetModules();
  });

  async function fatalOverBrokenLogger(thrown: unknown): Promise<string[]> {
    const emergencies: string[] = [];
    vi.resetModules();
    vi.doMock("../src/log.js", () => ({
      bindLog: () => ({
        debug: (): void => {},
        info: (): void => {},
        warn: (): void => {},
        error: (): void => {
          throw thrown;
        },
        logVerbose: (): void => {},
        with: (): unknown => ({}),
      }),
      emergencyStderr: (message: string): void => {
        emergencies.push(message);
      },
    }));
    const fatal = await import("../src/fatal.js");
    fatal.reportFatal(new Error("the original failure"));
    return emergencies;
  }

  it("routes the original failure to the emergency channel, naming the logger's error", async () => {
    // Arrange, Act.
    const emergencies = await fatalOverBrokenLogger(new Error("fd 3 is closed"));

    // Assert — the failure being reported is not swallowed by the failure to
    // report it.
    expect(emergencies).toHaveLength(1);
    expect(emergencies[0]).toContain("the original failure");
    expect(emergencies[0]).toContain("logger failure: fd 3 is closed");
  });

  it("stringifies a logger failure that is not an Error", async () => {
    // Arrange, Act.
    const emergencies = await fatalOverBrokenLogger("EBADF");

    // Assert.
    expect(emergencies[0]).toContain("logger failure: EBADF");
  });
});
