// @vitest-environment jsdom
// The webhost's own services: the gate and the load button drawn over the
// pages from the host's own daemon connection. See src/host/services.ts.
import { afterEach, describe, expect, it } from "vitest";

import { startWebhostServices, watchColdGates, type WebhostServicesDeps } from "../../src/host/services.js";
import type { Host } from "../../src/host/host.js";
import type { DaemonContext } from "../../src/rpc/context.js";
import { create } from "@bufbuild/protobuf";
import { RosterColdGateSchema, type WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { appContext, pushOf, roster, rosterStream } from "../sidebar/harness.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { quietWebhostRuntime } from "./webhost-runtime.js";
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
    shown: null as ((id: string) => void) | null,
    slots: null as ((id: string, pixels: number) => void) | null,
    dismissed: [] as string[],
  };
  const host = {
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
      reopen: () => undefined,
    };
  };
  return { watchColdGates, watched };
}

/** Start the services with DEPS over a quiet client, keeping the page-wide logger. */
function start(host: Host, deps: WebhostServicesDeps) {
  const { runtime, logged } = quietWebhostRuntime("host");
  stop = startWebhostServices(document, window, host, runtime, {
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
    const record = capture.sent.find((r) => r.operation === "failure.report");
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

describe("watchColdGates", () => {
  /** Every stream, its workspaces naming the cold gate of workspace GATED. */
  const withGate = (gated: string, revision: bigint) => {
    const streams = roster({ revision });
    streams.workspaces.coldGates = [create(RosterColdGateSchema, { workspace: { id: gated, dir: `/w/${gated}` } })];
    return streams;
  };

  /** Watch the cold gates over PUSHES, resolving once WANT gate lists were told. */
  function watching(pushes: Parameters<typeof rosterStream>[0], want: number) {
    const told: string[][] = [];
    let done: () => void = () => undefined;
    const settled = new Promise<void>((resolve) => (done = resolve));
    const handle = watchColdGates(appContext(rosterStream(pushes)), (gates) => {
      told.push(gates.map((gate) => gate.workspace?.id ?? ""));
      if (told.length === want) done();
    });
    return { told, settled, handle };
  }

  it("tells the gates of the workspaces stream the first push carries", async () => {
    // Arrange / Act
    const { told, settled, handle } = watching([withGate("ws-1", 1n)], 1);
    await settled;
    // Assert
    expect(told).toEqual([["ws-1"]]);
    handle.cancel();
  });

  it("tells nothing for a push that leaves the workspaces stream out", async () => {
    // Arrange: the middle push carries the merge queue alone; the last one's gates are told after it.
    const { told, settled, handle } = watching(
      [withGate("ws-1", 1n), pushOf(roster({ revision: 2n }), ["merge_queue"]), pushOf(withGate("ws-3", 3n), ["workspaces"])],
      2,
    );
    // Act
    await settled;
    // Assert
    expect(told).toEqual([["ws-1"], ["ws-3"]]);
    handle.cancel();
  });

  it("refuses a first push missing a stream, telling none of its gates, and logs why", async () => {
    // Arrange: the refused push, then a whole one, whose gates are told after it.
    const capture = captureLogRecords("error");
    const { told, settled, handle } = watching([pushOf(withGate("ws-bad", 1n), ["workspaces"]), withGate("ws-2", 2n)], 1);
    // Act
    await settled;
    // Assert
    const record = await forwardedRecord(capture, "rpc.stream-frame-undecodable");
    expect([told, record.context?.path]).toEqual([[["ws-2"]], "WatchWorkspaceRosterResponse.streams"]);
    handle.cancel();
  });
});
