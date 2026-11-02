// @vitest-environment jsdom
// What each of Emacs's two web views starts the same way: its connection, its
// logger, its failures, its load census and its daemon context. See
// src/host/document.ts.
import { afterEach, describe, expect, it } from "vitest";

import {
  startWebhostDocument,
  startWebhostRuntime,
  type StartedWebhostDocument,
  type WebhostDocumentDeps,
  type WebhostRuntime,
  type WebhostRuntimeDeps,
} from "../../src/host/document.js";
import { ForwardingLogger, log, setLogger } from "../../src/log.js";
import { captureLogRecords } from "../log-capture.js";
import { clientLogRecorder } from "../rpc/client-log-recorder.js";
import { quietWebhostRuntime } from "./webhost-runtime.js";
import { dispatchPageTransition } from "../page-transition.js";
import { beginPage, pageEnded, pageEnding, tearDownPage } from "../../src/page-teardown.js";

let started: StartedWebhostDocument | null = null;

afterEach(() => {
  started?.dispose();
  started = null;
  tearDownPage();
  beginPage();
});

/** Begin the sidebar's runtime with DEPS over a quiet client, keeping the page-wide logger. */
function runtime(deps: WebhostRuntimeDeps = {}) {
  const { client, logged } = clientLogRecorder();
  const made = startWebhostRuntime("sidebar", window, { client, installLogger: () => undefined, ...deps });
  return { made, logged };
}

/** Start the sidebar's document with DEPS on a quiet runtime; answers the runtime. */
function start(deps: WebhostDocumentDeps = {}): WebhostRuntime {
  const made = quietWebhostRuntime("sidebar").runtime;
  started = startWebhostDocument(made, document, window, {
    installCensus: () => ({ stop: () => undefined }),
    webappBuild: () => "Web1",
    ...deps,
  });
  return made;
}

/** Install the runtime's logger page-wide, answering it for a flush. */
function installing() {
  let installed: ForwardingLogger | null = null;
  return {
    installLogger: (logger: ForwardingLogger) => {
      installed = logger;
      setLogger(logger);
    },
    flush: () => (installed)?.flush(),
  };
}

describe("startWebhostRuntime", () => {
  it("files its records as the webhost's", async () => {
    // Arrange
    const logger = installing();
    const { logged } = runtime({ installLogger: logger.installLogger });

    // Act
    log.info("a document record", { operation: "test.webhost-record", context: {} });
    logger.flush();
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    expect(logged.length > 0 && logged.every((r) => r.source.case === "webhost")).toBe(true);
  });

  it("names the document in every record it writes", () => {
    // Arrange
    const capture = captureLogRecords();
    runtime();

    // Act
    log.info("a document record", { operation: "test.webhost-document", context: {} });

    // Assert
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "test.webhost-document");
    expect(record?.context?.["webhost_document"]).toBe("sidebar");
  });

  it("tells every record observer every record it writes", () => {
    // Arrange
    const seen: string[] = [];
    const logger = installing();
    const { made } = runtime({ installLogger: logger.installLogger });
    made.onRecord((level, line) => seen.push(`${level}:${(JSON.parse(line) as { operation: string }).operation}`));

    // Act
    log.warn("a warning", { operation: "test.observed", context: {} });

    // Assert
    expect(seen).toContain("warn:test.observed");
  });

  it("stops telling an observer once its stop is called", () => {
    // Arrange
    const seen: string[] = [];
    const logger = installing();
    const { made } = runtime({ installLogger: logger.installLogger });
    const stop = made.onRecord((level) => seen.push(level));

    // Act
    stop();
    log.warn("a warning", { operation: "test.observed", context: {} });

    // Assert
    expect(seen).toEqual([]);
  });

  it("starts at info, on the daemon that served it", () => {
    // Act
    const { made } = runtime();

    // Assert
    expect([made.daemon.current(), made.document]).toEqual([window.location.origin, "sidebar"]);
  });
});

describe("startWebhostDocument", () => {
  it("tells its context's streams when the daemon it dials moves", () => {
    // Arrange
    const { daemon } = start();
    let moves = 0;
    started?.ctx.onDaemonMoved(() => (moves += 1));

    // Act
    daemon.set("http://host.localhost:9");

    // Assert
    expect(moves).toBe(1);
  });

  it("installs the load census on its document", () => {
    // Arrange
    const installedOn: Document[] = [];

    // Act
    start({
      installCensus: (doc) => {
        installedOn.push(doc);
        return { stop: () => undefined };
      },
    });

    // Assert
    expect(installedOn).toEqual([document]);
  });

  it("ends, its requests aborted and its load census stopped, when it unloads for good", () => {
    // Arrange
    let stopped = 0;
    start({ installCensus: () => ({ stop: () => void stopped++ }) });
    const requests = pageEnding();

    // Act
    dispatchPageTransition("pagehide", false);

    // Assert
    expect([requests.aborted, stopped]).toEqual([true, 1]);
  });

  it("keeps running when it is kept for the back/forward cache", () => {
    // Arrange
    start();

    // Act
    dispatchPageTransition("pagehide", true);

    // Assert
    expect(pageEnded()).toBe(false);
  });

  it("no longer ends the page at an unload once disposed", () => {
    // Arrange
    start();
    started?.dispose();
    started = null;

    // Act
    dispatchPageTransition("pagehide", false);

    // Assert
    expect(pageEnded()).toBe(false);
  });

  it("stops the load census when disposed", () => {
    // Arrange
    let stopped = 0;
    start({ installCensus: () => ({ stop: () => void stopped++ }) });

    // Act
    started?.dispose();
    started = null;

    // Assert
    expect(stopped).toBe(1);
  });
});
