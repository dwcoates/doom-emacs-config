// @vitest-environment jsdom
// The webhost's own services: the gate and the load button drawn over the
// pages from the host's own daemon connection, its failures relayed to every
// page. See src/host/services.ts.
import { afterEach, describe, expect, it } from "vitest";

import type { WebhostFailure } from "../../src/host/protocol.js";
import { startWebhostServices, type WebhostServicesDeps } from "../../src/host/services.js";
import type { Host } from "../../src/host/host.js";
import { daemonUnreachable } from "../../src/failure/sink.js";
import type { DaemonContext } from "../../src/rpc/context.js";
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { captureLogRecords } from "../log-capture.js";
import { clientLogRecorder } from "../rpc/client-log-recorder.js";
import { beginPage, tearDownPage } from "../../src/page-teardown.js";

let stop: { dispose(): void } | null = null;

afterEach(() => {
  stop?.dispose();
  stop = null;
  tearDownPage();
  beginPage();
});

/** A webhost that records what the services tell it. */
function recordingHost() {
  const told = {
    failures: [] as Array<readonly WebhostFailure[]>,
    shown: null as ((id: string) => void) | null,
    slots: null as ((id: string, pixels: number) => void) | null,
    dismissed: [] as string[],
  };
  const host = {
    setFailures: (failures: readonly WebhostFailure[]) => told.failures.push(failures),
    onShown: (listener: (id: string) => void) => {
      told.shown = listener;
      return () => {
        told.shown = null;
      };
    },
    onGateSlot: (listener: (id: string, pixels: number) => void) => {
      told.slots = listener;
      return () => {
        told.slots = null;
      };
    },
    onHeldBackShown: () => () => undefined,
    dismissDigest: (id: string) => {
      told.dismissed.push(id);
      return "";
    },
  } as unknown as Host;
  return { host, told };
}

/** A cold-gate watch that hands the test its listener and its context, and records its cancel. */
function recordingColdGates() {
  const watched = {
    onGates: null as ((gates: WorkspaceRoster["coldGates"]) => void) | null,
    ctx: null as DaemonContext | null,
    cancelled: false,
  };
  const watchColdGates: WebhostServicesDeps["watchColdGates"] = (ctx, onGates) => {
    watched.ctx = ctx;
    watched.onGates = onGates;
    return {
      cancel: () => {
        watched.cancelled = true;
      },
    };
  };
  return { watchColdGates, watched };
}

/** Start the services with DEPS over a quiet client, keeping the page-wide logger. */
function start(host: Host, deps: WebhostServicesDeps) {
  const { client, logged } = clientLogRecorder();
  stop = startWebhostServices(document, window, host, {
    client,
    installLogger: () => undefined,
    installCensus: () => ({ stop: () => undefined }),
    webappBuild: () => "Web1",
    watchColdGates: recordingColdGates().watchColdGates,
    ...deps,
  });
  return logged;
}

/** A webhost gate that records what it is told. */
function recordingGate() {
  const told = {
    gates: [] as string[][],
    shown: [] as Array<string | null>,
    slots: [] as Array<[string, number]>,
    disposed: false,
    dismiss: null as ((id: string) => void) | null,
  };
  const createGate: WebhostServicesDeps["createGate"] = (env) => {
    told.dismiss = env.dismissDigest;
    return {
      setGates: (gates) => void told.gates.push(gates.map((g) => g.workspace?.id ?? "")),
      setShown: (id) => void told.shown.push(id),
      setSlot: (id, pixels) => void told.slots.push([id, pixels]),
      dispose: () => {
        told.disposed = true;
      },
    };
  };
  return { createGate, told };
}

describe("the webhost's gate", () => {
  it("is told the roster's cold gates on every roster pushed", () => {
    // Arrange
    const { host } = recordingHost();
    const { createGate, told } = recordingGate();
    const { watchColdGates, watched } = recordingColdGates();
    start(host, { gate: document.createElement("div"), createGate, watchColdGates });

    // Act
    watched.onGates?.([]);

    // Assert
    expect(told.gates).toEqual([[]]);
  });

  it("watches the roster's cold gates over the webhost's own context", () => {
    // Arrange
    const { host } = recordingHost();
    const { createGate } = recordingGate();
    const { watchColdGates, watched } = recordingColdGates();

    // Act
    start(host, { gate: document.createElement("div"), createGate, watchColdGates });

    // Assert
    expect(watched.ctx).not.toBeNull();
  });

  it("stops watching the roster's cold gates when stopped", () => {
    // Arrange
    const { host } = recordingHost();
    const { createGate } = recordingGate();
    const { watchColdGates, watched } = recordingColdGates();
    start(host, { gate: document.createElement("div"), createGate, watchColdGates });

    // Act
    stop?.dispose();
    stop = null;

    // Assert
    expect(watched.cancelled).toBe(true);
  });

  it("watches no cold gates without a gate to draw them", () => {
    // Arrange
    const { host } = recordingHost();
    const { watchColdGates, watched } = recordingColdGates();

    // Act
    start(host, { watchColdGates });

    // Assert
    expect(watched.onGates).toBeNull();
  });

  it("is told every page the webhost shows", () => {
    // Arrange
    const { host, told: hostTold } = recordingHost();
    const { createGate, told } = recordingGate();
    start(host, { gate: document.createElement("div"), createGate });

    // Act
    hostTold.shown?.("ws-b");

    // Assert
    expect(told.shown).toEqual(["ws-b"]);
  });

  it("is told every slot Emacs records", () => {
    // Arrange
    const { host, told: hostTold } = recordingHost();
    const { createGate, told } = recordingGate();
    start(host, { gate: document.createElement("div"), createGate });

    // Act
    hostTold.slots?.("ws-b", 132);

    // Assert
    expect(told.slots).toEqual([["ws-b", 132]]);
  });

  it("closes the shown page's news digest through the webhost", () => {
    // Arrange
    const { host, told: hostTold } = recordingHost();
    const { createGate, told } = recordingGate();
    start(host, { gate: document.createElement("div"), createGate });

    // Act
    told.dismiss?.("ws-b");

    // Assert
    expect(hostTold.dismissed).toEqual(["ws-b"]);
  });

  it("is disposed, and stops hearing slots, when the services stop", () => {
    // Arrange
    const { host, told: hostTold } = recordingHost();
    const { createGate, told } = recordingGate();
    start(host, { gate: document.createElement("div"), createGate });

    // Act
    stop?.dispose();
    stop = null;

    // Assert
    expect([told.disposed, hostTold.slots]).toEqual([true, null]);
  });

  it("is not created without an element to draw in", () => {
    // Arrange
    const { host } = recordingHost();
    let created = false;

    // Act
    start(host, {
      createGate: () => {
        created = true;
        throw new Error("no gate was wanted");
      },
    });

    // Assert
    expect(created).toBe(false);
  });
});

describe("startWebhostServices", () => {
  it("hands every page the failures its services file", () => {
    // Arrange
    const { host, told } = recordingHost();
    const { createGate } = recordingGate();
    const { watchColdGates, watched } = recordingColdGates();
    start(host, { gate: document.createElement("div"), createGate, watchColdGates });

    // Act
    watched.ctx?.failures.report(daemonUnreachable(1006, "gone"));

    // Assert
    expect(told.failures.at(-1)?.map((f) => f.arm)).toEqual(["daemonUnreachable"]);
  });

  it("lists services that failed to start in every page's chip", () => {
    // Arrange
    const { host, told } = recordingHost();

    // Act
    start(host, {
      gate: document.createElement("div"),
      createGate: () => {
        throw new Error("no card");
      },
    });

    // Assert
    expect(told.failures.at(-1)?.map((f) => f.arm)).toEqual(["bootFailed"]);
  });

  it("records services that failed to start at error", () => {
    // Arrange
    const capture = captureLogRecords();
    const { host } = recordingHost();

    // Act
    start(host, {
      gate: document.createElement("div"),
      createGate: () => {
        throw new Error("no card");
      },
    });

    // Assert
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "webhost.services-start-failed");
    expect([record?.level.case, record?.context?.["cause"]]).toEqual(["error", "Error: no card"]);
  });

  it("records its records as the host document's", () => {
    // Arrange
    const capture = captureLogRecords();
    const { host } = recordingHost();

    // Act
    start(host, {});

    // Assert
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "webhost.services-start");
    expect(record?.context?.["webhost_document"]).toBe("host");
  });
});
