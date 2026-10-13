// The webhost's failures, listed in a page's warning chip beside its own. See
// src/failure/webhost.ts.
import { describe, expect, it } from "vitest";

import { createLocalFailures } from "../../src/failure/local.js";
import { bootFailed, daemonUnreachable } from "../../src/failure/sink.js";
import { createWebhostFailureView, mergeFailureViews } from "../../src/failure/webhost.js";
import type { WebhostFailure } from "../../src/host/protocol.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

const UNREACHABLE: WebhostFailure = { arm: "daemonUnreachable", headline: "the webhost lost the daemon", evidence: [] };
const BOOT: WebhostFailure = { arm: "bootFailed", headline: "the sidebar did not start", evidence: [["cause", "x"]] };

describe("createWebhostFailureView", () => {
  it("stands nothing until the webhost says", () => {
    // Arrange / Act
    const view = createWebhostFailureView();

    // Assert
    expect(view.standing()).toEqual([]);
  });

  it("stands the set the webhost last stated", () => {
    // Arrange
    const view = createWebhostFailureView();
    view.set([UNREACHABLE, BOOT]);

    // Act
    view.set([BOOT]);

    // Assert
    expect(view.standing()).toEqual([BOOT]);
  });

  it("tells its listeners of each new set", () => {
    // Arrange
    const view = createWebhostFailureView();
    let told = 0;
    view.subscribe(() => {
      told += 1;
    });

    // Act
    view.set([BOOT]);

    // Assert
    expect(told).toBe(1);
  });

  it("records each new set at info with its arms", async () => {
    // Arrange
    const capture = captureLogRecords();
    const view = createWebhostFailureView();

    // Act
    view.set([BOOT]);

    // Assert
    const record = await forwardedRecord(capture, "warning-chip.webhost-failures");
    expect([record.level.case, record.context?.["arms"]]).toEqual(["info", ["bootFailed"]]);
  });
});

describe("mergeFailureViews", () => {
  it("lists the page's own failures, then the webhost's", () => {
    // Arrange
    const own = createLocalFailures();
    own.report(bootFailed("page"));
    const webhost = createWebhostFailureView();
    webhost.set([UNREACHABLE]);

    // Act
    const arms = mergeFailureViews(own, webhost).standing().map((f) => f.arm);

    // Assert
    expect(arms).toEqual(["bootFailed", "daemonUnreachable"]);
  });

  it("lists the page's own row once when both stand the same arm", () => {
    // Arrange
    const own = createLocalFailures();
    own.report(daemonUnreachable(1006, "gone"));
    const webhost = createWebhostFailureView();
    webhost.set([UNREACHABLE]);

    // Act
    const rows = mergeFailureViews(own, webhost).standing();

    // Assert
    expect(rows.map((f) => f.headline)).toEqual([own.standing()[0]?.headline]);
  });

  it("tells a listener when either side changes", () => {
    // Arrange
    const own = createLocalFailures();
    const webhost = createWebhostFailureView();
    let told = 0;
    mergeFailureViews(own, webhost).subscribe(() => {
      told += 1;
    });

    // Act
    own.report(bootFailed("page"));
    webhost.set([UNREACHABLE]);

    // Assert
    expect(told).toBe(2);
  });

  it("tells a listener nothing once unsubscribed", () => {
    // Arrange
    const own = createLocalFailures();
    const webhost = createWebhostFailureView();
    let told = 0;
    const off = mergeFailureViews(own, webhost).subscribe(() => {
      told += 1;
    });

    // Act
    off();
    own.report(bootFailed("page"));
    webhost.set([UNREACHABLE]);

    // Assert
    expect(told).toBe(0);
  });
});
