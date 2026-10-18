// @vitest-environment jsdom
// What each of the webapp's two documents sets up the same way. See
// src/document-runtime.ts.
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import type { ClientLogRequest } from "../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import { installForwardingLogger, installTextAffordances, quietAtUnload } from "../src/document-runtime.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";
import { clientLogRecorder } from "./rpc/client-log-recorder.js";
import { setLogger, log, type ForwardingLogger } from "../src/log.js";
import { dispatchPageTransition } from "./page-transition.js";

/** Install a forwarding logger as SOURCE at THRESHOLD, log at LEVEL, and answer what reached the daemon. */
async function forwardOne(threshold: "info" | "warn", level: "info" | "warn"): Promise<ClientLogRequest[]> {
  const { client, logged } = clientLogRecorder();
  let installed: ForwardingLogger | null = null;
  installForwardingLogger(() => client, { case: "webhost", value: {} }, threshold, (logger) => {
    installed = logger;
    setLogger(logger);
  });
  log[level]("one record", { operation: "test.document-runtime", context: {} });
  (installed as ForwardingLogger | null)?.flush();
  await new Promise((resolve) => setTimeout(resolve, 0));
  return logged;
}

describe("installForwardingLogger", () => {
  it("forwards each record to the daemon as the source it names", async () => {
    // Act
    const logged = await forwardOne("info", "info");

    // Assert
    expect(logged.map((r) => [r.source.case, r.record?.operation])).toEqual([["webhost", "test.document-runtime"]]);
  });

  it("keeps back a record below its threshold", async () => {
    // Act
    const logged = await forwardOne("warn", "info");

    // Assert
    expect(logged).toEqual([]);
  });

  it("answers the logger it installed", () => {
    // Arrange
    const { client } = clientLogRecorder();
    let installed: ForwardingLogger | null = null;

    // Act
    const answered = installForwardingLogger(() => client, { case: "webhost", value: {} }, "info", (logger) => {
      installed = logger;
    });

    // Assert
    expect(answered).toBe(installed);
  });
});

describe("installTextAffordances", () => {
  it("lets a click that ends a drag-select activate nothing", () => {
    // Arrange
    const uninstall = installTextAffordances(document);
    const button = document.createElement("button");
    button.textContent = "Send now";
    document.body.append(button);
    let reached = 0;
    button.addEventListener("click", () => reached++);
    const paragraph = document.createElement("p");
    paragraph.textContent = "frame unreadable";
    document.body.append(paragraph);
    button.dispatchEvent(new Event("pointerdown", { bubbles: true }));
    window.getSelection()?.selectAllChildren(paragraph);

    // Act
    button.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, detail: 1 }));

    // Assert
    expect(reached).toBe(0);
    uninstall();
    window.getSelection()?.removeAllRanges();
    button.remove();
    paragraph.remove();
  });

  it("copies a selection on C-c", () => {
    // Arrange
    const uninstall = installTextAffordances(document);
    let copied = 0;
    Object.defineProperty(document, "execCommand", {
      configurable: true,
      value: (command: string) => command === "copy" && ++copied > 0,
    });
    const paragraph = document.createElement("p");
    paragraph.textContent = "frame unreadable";
    document.body.append(paragraph);
    window.getSelection()?.selectAllChildren(paragraph);

    // Act
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "c", ctrlKey: true, bubbles: true }));

    // Assert
    expect(copied).toBe(1);
    uninstall();
    Reflect.deleteProperty(document, "execCommand");
    window.getSelection()?.removeAllRanges();
    paragraph.remove();
  });

  it("is what both documents set themselves up with", () => {
    // Arrange
    const here = path.dirname(fileURLToPath(import.meta.url));

    // Act
    const sources = ["main.ts", path.join("host", "sidebar.ts")].map((file) =>
      readFileSync(path.join(here, "..", "src", file), "utf8"),
    );

    // Assert
    expect(
      sources.map((s) => [
        s.includes("installForwardingLogger("),
        s.includes("installTextAffordances("),
        s.includes("new ForwardingLogger("),
        s.includes("installCopyFallback("),
        s.includes("installCopyChords("),
      ]),
    ).toEqual([
      [true, true, false, false, false],
      [true, true, false, false, false],
    ]);
  });
});


/** A context and a teardown that count what they were asked to do. */
function counted() {
  const seen = { quiesced: 0, torn: 0 };
  return { seen, ctx: { quiesce: () => void seen.quiesced++ }, teardown: () => void seen.torn++ };
}

describe("quietAtUnload", () => {
  it("quiets the context and runs the teardown when the document unloads for good", () => {
    // Arrange
    const { seen, ctx, teardown } = counted();
    const stop = quietAtUnload(document.defaultView!, ctx, teardown);
    // Act
    dispatchPageTransition("pagehide", false);
    stop();
    // Assert
    expect(seen).toEqual({ quiesced: 1, torn: 1 });
  });

  it("leaves a page kept for the back/forward cache running", () => {
    // Arrange
    const { seen, ctx, teardown } = counted();
    const stop = quietAtUnload(document.defaultView!, ctx, teardown);
    // Act
    dispatchPageTransition("pagehide", true);
    stop();
    // Assert
    expect(seen).toEqual({ quiesced: 0, torn: 0 });
  });

  it("does nothing at an unload once its stop has run", () => {
    // Arrange
    const { seen, ctx, teardown } = counted();
    quietAtUnload(document.defaultView!, ctx, teardown)();
    // Act
    dispatchPageTransition("pagehide", false);
    // Assert
    expect(seen).toEqual({ quiesced: 0, torn: 0 });
  });

  it("records the unload through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { ctx, teardown } = counted();
    const stop = quietAtUnload(document.defaultView!, ctx, teardown);
    // Act
    dispatchPageTransition("pagehide", false);
    stop();
    // Assert
    const record = await forwardedRecord(capture, "document.unload-quiet");
    expect(record.level.case).toBe("info");
  });
});
