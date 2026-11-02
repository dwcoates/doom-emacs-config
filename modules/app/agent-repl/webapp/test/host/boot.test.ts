// @vitest-environment jsdom
// The boot of Emacs's two web views: each document's logger before its code,
// and every stage of the boot recorded. See src/host/boot.ts.
import { beforeEach, describe, expect, it } from "vitest";

import { bootWebhostDocument, type BootWebhostDeps } from "../../src/host/boot.js";
import type { WebhostDocument, WebhostRuntime } from "../../src/host/document.js";
import { captureLogRecords, forwardedRecords, recordFor, type LogCapture } from "../log-capture.js";
import { quietWebhostRuntime } from "./webhost-runtime.js";

let capture: LogCapture;

/** A runtime start that records which document it began, keeping the captured logger. */
function recordingRuntimes() {
  const begun: WebhostDocument[] = [];
  const made: WebhostRuntime[] = [];
  const startRuntime: BootWebhostDeps["startRuntime"] = (document) => {
    begun.push(document);
    const { runtime } = quietWebhostRuntime(document);
    made.push(runtime);
    return runtime;
  };
  return { startRuntime, begun, made };
}

/** Boot DOCUMENT with LOAD and START, on a recording runtime. */
function boot<Code, Started>(
  load: () => Promise<Code>,
  start: (code: Code, runtime: WebhostRuntime) => Started,
  queued = 0,
  document: WebhostDocument = "host",
) {
  const runtimes = recordingRuntimes();
  const booted = bootWebhostDocument(document, window, () => queued, load, start, { startRuntime: runtimes.startRuntime });
  return { booted, runtimes };
}

/** The operations of every record the boot forwarded, in order. */
async function operations(): Promise<string[]> {
  return (await forwardedRecords(capture)).map((record) => record.operation);
}

/** The forwarded records at error, as [operation, context]. */
async function errors(): Promise<unknown[]> {
  return (await forwardedRecords(capture)).filter((r) => r.level.case === "error").map((r) => [r.operation, r.context]);
}

beforeEach(() => {
  capture = captureLogRecords();
});

describe("bootWebhostDocument", () => {
  it("records each stage of a boot that started, in order", async () => {
    // Act
    await boot(async () => "code", () => "started").booted;

    // Assert
    expect(await operations()).toEqual(["webhost.boot-importing", "webhost.boot-imported"]);
  });

  it("records how many calls Emacs had queued when its entry ran", async () => {
    // Act
    await boot(async () => "code", () => "started", 2).booted;

    // Assert
    expect(recordFor(await forwardedRecords(capture), "webhost.boot-importing").context).toMatchObject({ queued: 2 });
  });

  it("begins the document it boots", async () => {
    // Arrange
    const { booted, runtimes } = boot(async () => "code", () => "started", 0, "sidebar");

    // Act
    await booted;

    // Assert
    expect(runtimes.begun).toEqual(["sidebar"]);
  });

  it("starts the code it loaded with the runtime its logger was made with", async () => {
    // Arrange
    const handed: Array<[string, WebhostRuntime]> = [];
    const { booted, runtimes } = boot(
      async () => "code",
      (code, runtime) => {
        handed.push([code, runtime]);
      },
    );

    // Act
    await booted;

    // Assert
    expect(handed).toEqual([["code", runtimes.made[0]]]);
  });

  it("answers what its start answered", async () => {
    // Act
    const started = await boot(async () => "code", () => "the page").booted;

    // Assert
    expect(started).toBe("the page");
  });

  it("rejects with the import's own error when the code fails to import", async () => {
    // Arrange
    const failure = new TypeError("Importing a module script failed.");

    // Act
    const { booted } = boot(
      async () => {
        throw failure;
      },
      () => "started",
    );

    // Assert
    await expect(booted).rejects.toBe(failure);
  });

  it("records a failed import once, at error, naming its cause", async () => {
    // Act
    await boot(
      async () => {
        throw new TypeError("Importing a module script failed.");
      },
      () => "started",
    ).booted.catch(() => undefined);

    // Assert
    expect(await errors()).toEqual([
      ["webhost.boot-import-failed", expect.objectContaining({ cause: "TypeError: Importing a module script failed." })],
    ]);
  });

  it("starts nothing when the code failed to import", async () => {
    // Arrange
    let started = false;

    // Act
    await boot(
      async () => {
        throw new TypeError("Importing a module script failed.");
      },
      () => {
        started = true;
      },
    ).booted.catch(() => undefined);

    // Assert
    expect(started).toBe(false);
  });

  it("rejects with the start's own error when the document fails to start", async () => {
    // Arrange
    const failure = new Error("queued call 1 of 1 threw: TypeError: no such page");

    // Act
    const { booted } = boot(
      async () => "code",
      () => {
        throw failure;
      },
    );

    // Assert
    await expect(booted).rejects.toBe(failure);
  });

  it("records a failed start once, at error, naming its cause", async () => {
    // Act
    await boot(
      async () => "code",
      () => {
        throw new Error("queued call 1 of 1 threw: TypeError: no such page");
      },
    ).booted.catch(() => undefined);

    // Assert
    expect(await errors()).toEqual([
      ["webhost.boot-failed", expect.objectContaining({ cause: "Error: queued call 1 of 1 threw: TypeError: no such page" })],
    ]);
  });
});
