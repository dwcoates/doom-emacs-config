// @vitest-environment jsdom
// The webhost's own sidebar: started once for every workspace page, logging as
// the webhost, its failures relayed to every page. See src/host/sidebar.ts.
import { afterEach, describe, expect, it } from "vitest";

import type { WebhostFailure } from "../../src/host/protocol.js";
import { startWebhostSidebar, type WebhostSidebarStartDeps } from "../../src/host/sidebar.js";
import type { Host } from "../../src/host/host.js";
import { ForwardingLogger, log, setLogger } from "../../src/log.js";
import { daemonUnreachable } from "../../src/failure/sink.js";
import type { DaemonContext } from "../../src/rpc/context.js";
import { createDaemonOrigin } from "../../src/rpc/daemon-origin.js";
import { captureLogRecords } from "../log-capture.js";
import { clientLogRecorder } from "../rpc/client-log-recorder.js";
import { dispatchPageTransition } from "../page-transition.js";
import { beginPage, pageEnded, pageEnding, tearDownPage } from "../../src/page-teardown.js";

let stop: { dispose(): void } | null = null;

afterEach(() => {
  stop?.dispose();
  stop = null;
  tearDownPage();
  beginPage();
});

/** A webhost that records what the sidebar tells it. */
function recordingHost() {
  const told = {
    docked: [] as boolean[],
    failures: [] as Array<readonly WebhostFailure[]>,
    shown: null as ((id: string) => void) | null,
  };
  const host = {
    setSidebarDocked: (docked: boolean) => told.docked.push(docked),
    setFailures: (failures: readonly WebhostFailure[]) => told.failures.push(failures),
    onShown: (listener: (id: string) => void) => {
      told.shown = listener;
      return () => {
        told.shown = null;
      };
    },
  } as unknown as Host;
  return { host, told };
}

/** Start the sidebar with DEPS over a quiet client, keeping the page-wide logger. */
function start(host: Host, deps: WebhostSidebarStartDeps) {
  const { client, logged } = clientLogRecorder();
  stop = startWebhostSidebar(document, window, host, document.createElement("nav"), {
    client,
    installLogger: () => undefined,
    installCensus: () => ({ stop: () => undefined }),
    webappBuild: () => "Web1",
    ...deps,
  });
  return logged;
}

describe("startWebhostSidebar", () => {
  it("files its records as the webhost's", async () => {
    // Arrange
    const { host } = recordingHost();
    let installed: ForwardingLogger | null = null;
    const logged = start(host, {
      installLogger: (logger) => {
        installed = logger;
        setLogger(logger);
      },
      mount: () => ({ dispose: () => undefined, markShown: () => undefined }),
    });

    // Act
    log.info("a sidebar record", { operation: "test.webhost-record", context: {} });
    (installed as ForwardingLogger | null)?.flush();
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    expect(logged.map((r) => r.source.case)).toContain("webhost");
    expect(logged.every((r) => r.source.case === "webhost")).toBe(true);
  });

  it("docks the sidebar over every page once it has drawn", () => {
    // Arrange
    const { host, told } = recordingHost();

    // Act
    start(host, {
      mount: (_nav, _ctx, deps) => {
        deps?.revealed?.();
        return { dispose: () => undefined, markShown: () => undefined };
      },
    });

    // Assert
    expect(told.docked).toEqual([true]);
  });

  it("does not dock a sidebar that has not drawn", () => {
    // Arrange
    const { host, told } = recordingHost();

    // Act
    start(host, { mount: () => ({ dispose: () => undefined, markShown: () => undefined }) });

    // Assert
    expect(told.docked).toEqual([]);
  });

  it("hands every page the failures its sidebar files", () => {
    // Arrange
    const { host, told } = recordingHost();
    let ctx: DaemonContext | null = null;
    start(host, {
      mount: (_nav, c) => {
        ctx = c;
        return { dispose: () => undefined, markShown: () => undefined };
      },
    });

    // Act
    (ctx as DaemonContext | null)?.failures.report(daemonUnreachable(1006, "gone"));

    // Assert
    expect(told.failures.at(-1)?.map((f) => f.arm)).toEqual(["daemonUnreachable"]);
  });

  it("tells its context's streams when the daemon it dials moves", () => {
    // Arrange
    const { host } = recordingHost();
    const daemon = createDaemonOrigin("http://host.localhost:1");
    let ctx: DaemonContext | null = null;
    start(host, {
      daemon,
      mount: (_nav, c) => {
        ctx = c;
        return { dispose: () => undefined, markShown: () => undefined };
      },
    });
    let moves = 0;
    (ctx as DaemonContext | null)?.onDaemonMoved(() => (moves += 1));

    // Act
    daemon.set("http://host.localhost:2");

    // Assert
    expect(moves).toBe(1);
  });

  it("lists a sidebar that failed to start in every page's chip", () => {
    // Arrange
    const { host, told } = recordingHost();

    // Act
    start(host, {
      mount: () => {
        throw new Error("no roster");
      },
    });

    // Assert
    expect(told.failures.at(-1)?.map((f) => f.arm)).toEqual(["bootFailed"]);
  });

  it("records a sidebar that failed to start at error", () => {
    // Arrange
    const capture = captureLogRecords();
    const { host } = recordingHost();

    // Act
    start(host, {
      mount: () => {
        throw new Error("no roster");
      },
    });

    // Assert
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "webhost.sidebar-start-failed");
    expect([record?.level.case, record?.context?.["cause"]]).toEqual(["error", "Error: no roster"]);
  });

  it("moves the rail's highlight to every page the webhost shows", () => {
    // Arrange
    const { host, told } = recordingHost();
    const marked: string[] = [];
    start(host, { mount: () => ({ dispose: () => undefined, markShown: (id) => void marked.push(id) }) });

    // Act
    told.shown?.("ws-b");

    // Assert
    expect(marked).toEqual(["ws-b"]);
  });

  it("stops following the shown page once stopped", () => {
    // Arrange
    const { host, told } = recordingHost();
    start(host, { mount: () => ({ dispose: () => undefined, markShown: () => undefined }) });

    // Act
    stop?.dispose();
    stop = null;

    // Assert
    expect(told.shown).toBeNull();
  });

  it("closes the sidebar when stopped", () => {
    // Arrange
    const { host } = recordingHost();
    let disposed = false;
    start(host, {
      mount: () => ({
        dispose: () => {
          disposed = true;
        },
        markShown: () => undefined,
      }),
    });

    // Act
    stop?.dispose();
    stop = null;

    // Assert
    expect(disposed).toBe(true);
  });

  it("installs the load census on the webhost's document", () => {
    // Arrange
    const { host } = recordingHost();
    const installedOn: Document[] = [];

    // Act
    start(host, {
      mount: () => ({ dispose: () => undefined, markShown: () => undefined }),
      installCensus: (doc) => {
        installedOn.push(doc);
        return { stop: () => undefined };
      },
    });

    // Assert
    expect(installedOn).toEqual([document]);
  });

  it("ends the webhost, its requests aborted and its load census stopped, when it unloads for good", () => {
    // Arrange
    const { host } = recordingHost();
    let stopped = 0;
    start(host, {
      mount: () => ({ dispose: () => undefined, markShown: () => undefined }),
      installCensus: () => ({ stop: () => void stopped++ }),
    });
    const requests = pageEnding();

    // Act
    dispatchPageTransition("pagehide", false);

    // Assert
    expect([requests.aborted, stopped]).toEqual([true, 1]);
  });

  it("leaves the webhost running when it is kept for the back/forward cache", () => {
    // Arrange
    const { host } = recordingHost();
    start(host, { mount: () => ({ dispose: () => undefined, markShown: () => undefined }) });

    // Act
    dispatchPageTransition("pagehide", true);

    // Assert
    expect(pageEnded()).toBe(false);
  });

  it("no longer ends the page at an unload once stopped", () => {
    // Arrange
    const { host } = recordingHost();
    start(host, { mount: () => ({ dispose: () => undefined, markShown: () => undefined }) });
    stop?.dispose();
    stop = null;

    // Act
    dispatchPageTransition("pagehide", false);

    // Assert
    expect(pageEnded()).toBe(false);
  });

  it("stops the load census when stopped", () => {
    // Arrange
    const { host } = recordingHost();
    let stopped = 0;
    start(host, {
      mount: () => ({ dispose: () => undefined, markShown: () => undefined }),
      installCensus: () => ({ stop: () => void stopped++ }),
    });

    // Act
    stop?.dispose();
    stop = null;

    // Assert
    expect(stopped).toBe(1);
  });
});

