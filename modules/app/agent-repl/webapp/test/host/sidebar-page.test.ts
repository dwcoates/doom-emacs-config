// @vitest-environment jsdom
// The sidebar page's start: the rail in a web view of its own, handed to Emacs
// as `window.agentReplSidebar`. See src/host/sidebar-page.ts.
import { afterEach, describe, expect, it } from "vitest";

import { FOCUS_LEFT_EMACS, type FocusLeftSource } from "../../src/focus-left.js";
import { LevelWindow } from "../../../agent-shim/logging/ts/level-window.js";
import { SIDEBAR_ID, SIDEBAR_ROOT_CLASS, startSidebarPage, type StartSidebarPageDeps } from "../../src/host/sidebar-page.js";
import { log, setLogger, type ForwardingLogger } from "../../src/log.js";
import { createDaemonOrigin } from "../../src/rpc/daemon-origin.js";
import type { DaemonContext } from "../../src/rpc/context.js";
import { createSidebarNotices } from "../../src/sidebar/notices.js";
import { fakePopupChannel, quietPopupChannel } from "./popup-channel.js";
import type { WebhostSidebarDeps } from "../../src/sidebar/sidebar.js";
import { captureLogRecords } from "../log-capture.js";
import { clientLogRecorder } from "../rpc/client-log-recorder.js";
import { beginPage, tearDownPage } from "../../src/page-teardown.js";
import { levelApplyShapeOf } from "./level-apply-shape.js";

afterEach(() => {
  document.documentElement.classList.remove(SIDEBAR_ROOT_CLASS);
  delete window.agentReplSidebar;
  delete window.__agentReplSidebarPending;
  document.body.replaceChildren();
  document.head.replaceChildren();
  tearDownPage();
  beginPage();
});

/** A rail that records what the page tells it. */
function recordingRail() {
  const told = { marked: [] as string[], deps: null as WebhostSidebarDeps | null, nav: null as HTMLElement | null, ctx: null as DaemonContext | null };
  const mount: StartSidebarPageDeps["mount"] = (nav, ctx, deps) => {
    told.nav = nav;
    told.ctx = ctx;
    told.deps = deps ?? null;
    return { dispose: () => undefined, markShown: (id) => void told.marked.push(id) };
  };
  return { mount, told };
}

/** Start the page with DEPS over a quiet client. */
function start(deps: StartSidebarPageDeps = {}) {
  const { client } = clientLogRecorder();
  return startSidebarPage(document, window, {
    client,
    installLogger: () => undefined,
    installCensus: () => ({ stop: () => undefined }),
    webappBuild: () => "Web1",
    mount: recordingRail().mount,
    popupChannel: quietPopupChannel(),
    ...deps,
  });
}

/** The outcome a call answered. */
function outcome(answer: string): string {
  return (JSON.parse(answer) as { outcome: string }).outcome;
}

describe("startSidebarPage", () => {
  it("clears the workspace page's shell markup", () => {
    // Arrange
    document.body.innerHTML = '<div id="main-col"></div>';

    // Act
    start();

    // Assert
    expect(document.getElementById("main-col")).toBeNull();
  });

  it("marks the root as the sidebar page", () => {
    // Act
    start();

    // Assert
    expect(document.documentElement.classList.contains(SIDEBAR_ROOT_CLASS)).toBe(true);
  });

  it("installs the status colors' values", () => {
    // Act
    start();

    // Assert
    expect(document.head.querySelector('style[data-source="render-colors.json#color_values"]')).not.toBeNull();
  });

  it("mounts the rail in its hidden nav", () => {
    // Arrange
    const { mount, told } = recordingRail();

    // Act
    start({ mount });

    // Assert
    expect([told.nav?.id, told.nav?.hidden, told.nav?.parentElement]).toEqual([SIDEBAR_ID, true, document.body]);
  });

  it("hands the rail its failures and notices for the version", () => {
    // Arrange
    const { mount, told } = recordingRail();
    const notices = createSidebarNotices();

    // Act
    start({ mount, notices });

    // Assert
    expect(told.deps?.notices?.notices).toBe(notices);
  });

  it("hands the rail a mirror that draws its open panels over the feed, on the page's channel", () => {
    // Arrange
    const { mount, told } = recordingRail();
    const { channel, posted } = fakePopupChannel();
    const detail = document.createElement("div");
    const line = document.createElement("div");

    // Act
    start({ mount, popupChannel: channel });
    told.deps?.popupMirror?.show("ws-a", detail, line);

    // Assert
    expect((posted[0] as { kind: string }).kind).toBe("show");
  });

  it("hands the page to Emacs", () => {
    // Act
    const page = start();

    // Assert
    expect(window.agentReplSidebar).toBe(page);
  });

  it("runs the calls Emacs queued before it started, in order", () => {
    // Arrange
    const order: string[] = [];
    window.__agentReplSidebarPending = [
      (page) => order.push(outcome(page.markShown("ws-a"))),
      (page) => order.push(outcome(page.setDaemon("nope"))),
    ];

    // Act
    start();

    // Assert
    expect(order).toEqual(["marked", "refused"]);
  });

  it("leaves no queue behind once it has run it", () => {
    // Arrange
    window.__agentReplSidebarPending = [];

    // Act
    start();

    // Assert
    expect(window.__agentReplSidebarPending).toBeUndefined();
  });

  it("counts the warnings it records in its notices", () => {
    // Arrange
    const notices = createSidebarNotices();
    start({
      notices,
      installLogger: (logger: ForwardingLogger) => setLogger(logger),
    });

    // Act
    log.warn("careful", { operation: "test.sidebar-warn", context: {} });

    // Assert
    expect(notices.tone()).toBe("warn");
  });

  it("lists a rail that failed to start behind the version", () => {
    // Arrange
    const { mount, told } = recordingRail();

    // Act
    start({
      mount: (nav, ctx, deps) => {
        mount(nav, ctx, deps);
        throw new Error("no roster");
      },
    });

    // Assert
    expect(told.deps?.notices?.failures.standing().map((f) => f.arm)).toEqual(["bootFailed"]);
  });

  it("records a rail that failed to start at error", () => {
    // Arrange
    const capture = captureLogRecords();

    // Act
    start({
      mount: () => {
        throw new Error("no roster");
      },
    });

    // Assert
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "sidebar-page.start-failed");
    expect([record?.level.case, record?.context?.["cause"]]).toEqual(["error", "Error: no roster"]);
  });
});

describe("markShown", () => {
  it("moves the rail's highlight to the page the host shows", () => {
    // Arrange
    const { mount, told } = recordingRail();
    const page = start({ mount });

    // Act
    const answer = page.markShown("ws-b");

    // Assert
    expect([outcome(answer), told.marked]).toEqual(["marked", ["ws-b"]]);
  });

  it("refuses an empty id, with a note saying so", () => {
    // Arrange
    const { mount, told } = recordingRail();
    const page = start({ mount });

    // Act
    const answer = JSON.parse(page.markShown("")) as { outcome: string; notes: { level: string }[] };

    // Assert
    expect([answer.outcome, answer.notes.map((n) => n.level), told.marked]).toEqual(["refused", ["warn"], []]);
  });
});

describe("setDaemon", () => {
  it("points the rail's calls at the daemon named", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");
    const page = start({ daemon });

    // Act
    const answer = page.setDaemon("http://host.localhost:2");

    // Assert
    expect([outcome(answer), daemon.current()]).toEqual(["retargeted", "http://host.localhost:2"]);
  });

  it("answers unchanged for the daemon it already dials", () => {
    // Arrange
    const page = start({ daemon: createDaemonOrigin("http://host.localhost:1") });

    // Act
    const answer = page.setDaemon("http://host.localhost:1");

    // Assert
    expect(outcome(answer)).toBe("unchanged");
  });

  it("refuses what is no daemon origin, leaving the daemon where it was", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");
    const page = start({ daemon });

    // Act
    const answer = page.setDaemon("ftp://elsewhere");

    // Assert
    expect([outcome(answer), daemon.current()]).toEqual(["refused", "http://host.localhost:1"]);
  });
});

describe("setLogLevel", () => {
  it("opens a level window until the end named", () => {
    // Arrange
    const level = LevelWindow.fixed("info");
    const page = start({ level });

    // Act
    const answer = page.setLogLevel("debug", Date.now() + 60_000);

    // Assert
    expect([outcome(answer), level.allows("debug").allowed]).toEqual(["opened", true]);
  });

  it("closes the window at info", () => {
    // Arrange
    const level = LevelWindow.fixed("info");
    const page = start({ level });
    page.setLogLevel("debug", Date.now() + 60_000);

    // Act
    const answer = page.setLogLevel("info", null);

    // Assert
    expect([outcome(answer), level.allows("debug").allowed]).toEqual(["closed", false]);
  });

  it("refuses a level it does not know", () => {
    // Arrange
    const page = start();

    // Act
    const answer = page.setLogLevel("trace", Date.now() + 60_000);

    // Assert
    expect(outcome(answer)).toBe("refused");
  });

  it("refuses a window that has already ended", () => {
    // Arrange
    const page = start();

    // Act
    const answer = page.setLogLevel("debug", 1);

    // Assert
    expect(outcome(answer)).toBe("refused");
  });

  it("applies the level through the level window, never by hand", () => {
    // Act
    const shape = levelApplyShapeOf("host/sidebar-page.ts");

    // Assert
    expect(shape).toEqual({ applies: true, handRolls: false });
  });
});

describe("focusMoved", () => {
  it("tells the rail Emacs moved its focus", () => {
    // Arrange
    const told: string[] = [];
    const focusLeft: FocusLeftSource = {
      onLeft: () => () => undefined,
      left: (why) => void told.push(why),
      dispose: () => undefined,
    };
    const page = start({ focusLeft });

    // Act
    const answer = page.focusMoved();

    // Assert
    expect([outcome(answer), told]).toEqual(["sent", [FOCUS_LEFT_EMACS]]);
  });

  it("hands the rail the focus-left signal it fires", () => {
    // Arrange
    const focusLeft: FocusLeftSource = { onLeft: () => () => undefined, left: () => undefined, dispose: () => undefined };
    const { mount, told } = recordingRail();

    // Act
    start({ mount, focusLeft });

    // Assert
    expect(told.deps?.focusLeft).toBe(focusLeft);
  });
});
