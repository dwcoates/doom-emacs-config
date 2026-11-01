// @vitest-environment jsdom
// What each of Emacs's two web views starts the same way: its connection, its
// logger, its failures, its load census and its daemon context. See
// src/host/document.ts.
import { afterEach, describe, expect, it } from "vitest";

import { startWebhostDocument, type StartedWebhostDocument, type WebhostDocumentDeps } from "../../src/host/document.js";
import { ForwardingLogger, log, setLogger } from "../../src/log.js";
import { createDaemonOrigin } from "../../src/rpc/daemon-origin.js";
import { captureLogRecords } from "../log-capture.js";
import { clientLogRecorder } from "../rpc/client-log-recorder.js";
import { dispatchPageTransition } from "../page-transition.js";
import { beginPage, pageEnded, pageEnding, tearDownPage } from "../../src/page-teardown.js";

let started: StartedWebhostDocument | null = null;

afterEach(() => {
  started?.dispose();
  started = null;
  tearDownPage();
  beginPage();
});

/** Start the sidebar's document with DEPS over a quiet client, keeping the page-wide logger. */
function start(deps: WebhostDocumentDeps = {}) {
  const { client, logged } = clientLogRecorder();
  started = startWebhostDocument("sidebar", document, window, {
    client,
    installLogger: () => undefined,
    installCensus: () => ({ stop: () => undefined }),
    webappBuild: () => "Web1",
    ...deps,
  });
  return logged;
}

describe("startWebhostDocument", () => {
  it("files its records as the webhost's", async () => {
    // Arrange
    let installed: ForwardingLogger | null = null;
    const logged = start({
      installLogger: (logger) => {
        installed = logger;
        setLogger(logger);
      },
    });

    // Act
    log.info("a document record", { operation: "test.webhost-record", context: {} });
    (installed as ForwardingLogger | null)?.flush();
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    expect(logged.length > 0 && logged.every((r) => r.source.case === "webhost")).toBe(true);
  });

  it("names the document in every record it writes", () => {
    // Arrange
    const capture = captureLogRecords();
    start();

    // Act
    log.info("a document record", { operation: "test.webhost-document", context: {} });

    // Assert
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "test.webhost-document");
    expect(record?.context?.["webhost_document"]).toBe("sidebar");
  });

  it("tells its record observer every record it writes", () => {
    // Arrange
    const seen: string[] = [];
    let installed: ForwardingLogger | null = null;
    start({
      installLogger: (logger) => {
        installed = logger;
        setLogger(logger);
      },
      onRecord: (level, line) => seen.push(`${level}:${(JSON.parse(line) as { operation: string }).operation}`),
    });

    // Act
    log.warn("a warning", { operation: "test.observed", context: {} });

    // Assert
    expect(installed).not.toBeNull();
    expect(seen).toContain("warn:test.observed");
  });

  it("tells its context's streams when the daemon it dials moves", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");
    start({ daemon });
    let moves = 0;
    started?.ctx.onDaemonMoved(() => (moves += 1));

    // Act
    daemon.set("http://host.localhost:2");

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
