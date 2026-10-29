// @vitest-environment jsdom
// A workspace page's side of the host protocol. See src/host/embed.ts.
import { createEditorIngressView, type EditorIngressView } from "../../src/held/editor-ingress.js";
import { LevelWindow } from "../../../agent-shim/logging/ts/level-window.js";
import { levelApplyShapeOf } from "./level-apply-shape.js";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { CONVERSATION_DRAWN_ATTRIBUTE } from "../../src/feed/conversation-drawn.js";
import { FOCUS_LEFT_EMACS, WEBHOST_SIDEBAR_CLASS, connectToHost, type HostLink } from "../../src/host/embed.js";
import { HOST_CHANNEL, type PageToHost } from "../../src/host/protocol.js";
import { createPresence, type PresenceSource } from "../../src/presence.js";
import { createFocusLeft, type FocusLeftSource } from "../../src/focus-left.js";
import { createWebhostFailureView, type WebhostFailureView } from "../../src/failure/webhost.js";
import { captureLogRecords, type LogCapture } from "../log-capture.js";
import { createRestyleTurns, type RestyleTurnsSource } from "../../src/host/restyle-turn.js";

let pageWin: Window;
let pageDoc: Document;
let hostWin: Window;
let presence: PresenceSource;
let levelWindow: LevelWindow;
let clock: number;
let webhostFailures: WebhostFailureView;
let editorIngress: EditorIngressView;
let focusLeft: FocusLeftSource;
let restyleTurns: RestyleTurnsSource;
let leftFor: string[];
let link: HostLink | null;
let received: PageToHost[];
let capture: LogCapture;

/**
 * A stand-in for the page's own window inside the host's iframe: jsdom gives a
 * real iframe a `parent` that is not the test's window and delivers its posts
 * with no source, so the page's window and its host are modeled directly. The
 * page's DOCUMENT is the test's real one.
 */
function pageWindow(host: Window): Window {
  const target = new EventTarget();
  return Object.assign(target, {
    parent: host,
    getSelection: () => window.getSelection(),
  }) as unknown as Window;
}

/** A stand-in for the host's window: it records what the page posts to it. */
function hostWindow(): Window {
  return {
    postMessage: (message: unknown) => {
      received.push(message as PageToHost);
    },
  } as unknown as Window;
}

/** Posts are recorded synchronously by the stand-in host. */
async function delivered(): Promise<void> {
  await Promise.resolve();
}

/** Deliver DATA to the page as though SOURCE (by default its host) posted it. */
function fromHost(data: unknown, source: Window = hostWin): void {
  const event = new MessageEvent("message", { data });
  Object.defineProperty(event, "source", { value: source });
  pageWin.dispatchEvent(event);
}

async function operations(): Promise<string[]> {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent.map((record) => record.operation);
}

beforeEach(() => {
  capture = captureLogRecords("debug");
  received = [];
  hostWin = hostWindow();
  pageWin = pageWindow(hostWin);
  pageDoc = document;
  presence = createPresence(pageDoc, true);
  webhostFailures = createWebhostFailureView();
  editorIngress = createEditorIngressView();
  focusLeft = createFocusLeft(pageWin, presence);
  restyleTurns = createRestyleTurns();
  leftFor = [];
  focusLeft.onLeft((why) => leftFor.push(why));
  clock = 1_000_000;
  levelWindow = new LevelWindow({ level: "info", untilMs: null }, () => clock);
  link = connectToHost({ doc: pageDoc, win: pageWin, presence, webhostFailures, levelWindow, editorIngress, focusLeft, restyleTurns });
});

afterEach(() => {
  link?.dispose();
  focusLeft.dispose();
  presence.dispose();
  pageDoc.documentElement.removeAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
  pageDoc.documentElement.removeAttribute("style");
  pageDoc.documentElement.classList.remove(WEBHOST_SIDEBAR_CLASS);
  pageDoc.body.replaceChildren();
});

describe("joining", () => {
  it("answers no link for a page in no host", () => {
    // Arrange
    const alone = createPresence(document, false);

    // Act
    const none = connectToHost({ doc: document, win: window, presence: alone, webhostFailures, levelWindow, editorIngress, focusLeft, restyleTurns });


    // Assert
    expect(none).toBeNull();
    alone.dispose();
  });

  it("says hello to its host", async () => {
    // Act
    await delivered();

    // Assert
    expect(received).toContainEqual({ channel: HOST_CHANNEL, kind: "hello" });
  });

  it("records that it joined its host", async () => {
    expect(await operations()).toContain("page.host-joined");
  });
});

describe("restyle turns", () => {
  it("asks the host for a page's turn", async () => {
    // Act
    restyleTurns.request(() => {});
    // Assert
    await delivered();
    expect(received).toContainEqual({ channel: HOST_CHANNEL, kind: "restyleTurn" });
  });

  it("runs the waiting work when the host grants the turn, and ends it", async () => {
    // Arrange
    const ran: string[] = [];
    restyleTurns.request(() => ran.push("ran"));
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "restyleGranted" });
    // Assert
    await delivered();
    expect([ran, received.filter((m) => (m as { kind?: string }).kind === "restyled").length]).toEqual([["ran"], 1]);
  });
});

describe("presence", () => {
  it("takes the host showing it", () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "presence", shown: true });

    // Assert
    expect(presence.shown()).toBe(true);
  });

  it("hands the presence the start of the switch that showed it", () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "presence", shown: true, switchStartedAtMs: 1_700_000_000_123 });

    // Assert
    expect(presence.switchStartedAt()).toBe(1_700_000_000_123);
  });

  it("hands the presence the start of the reload that loaded it, shown or not", () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "presence", shown: false, reloadStartedAtMs: 1_700_000_000_500 });

    // Assert
    expect(presence.reloadStartedAt()).toBe(1_700_000_000_500);
  });

  it("records the host showing it at info", async () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "presence", shown: true });

    // Assert
    await operations();
    expect(capture.sent.find((r) => r.operation === "page.host-presence")?.level.case).toBe("info");
  });

  it("records nothing when the host repeats what it said", async () => {
    // Arrange
    fromHost({ channel: HOST_CHANNEL, kind: "presence", shown: false });

    // Act / Assert
    expect(await operations()).not.toContain("page.host-presence");
  });

  it("ignores a message from a window other than its host", () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "presence", shown: true }, pageWin);

    // Assert
    expect(presence.shown()).toBe(false);
  });

  it("records a host message from another build at info", async () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "teleport" });

    // Assert
    await operations();
    const record = capture.sent.find((r) => r.operation === "page.host-message-unreadable");
    expect(record?.level.case).toBe("info");
  });
});


describe("commands", () => {
  it("closes a standing news digest", () => {
    // Arrange
    const digest = pageDoc.createElement("div");
    digest.setAttribute("data-component", "news-digest");
    const close = pageDoc.createElement("button");
    close.setAttribute("data-news-digest-close", "");
    let clicked = false;
    close.addEventListener("click", () => {
      clicked = true;
    });
    digest.append(close);
    pageDoc.body.append(digest);

    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "dismissDigest" });

    // Assert
    expect(clicked).toBe(true);
  });
});

describe("a bubble expanded over the webapp", () => {
  it("dismissDigest closes a standing bubble overlay", () => {
    // Arrange
    const close = pageDoc.createElement("button");
    close.setAttribute("data-bubble-overlay-close", "");
    let clicked = false;
    close.addEventListener("click", () => {
      clicked = true;
    });
    pageDoc.body.append(close);

    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "dismissDigest" });

    // Assert
    expect(clicked).toBe(true);
  });

  it.each([
    ["opens", true],
    ["closes", false],
  ])("reports the overlay when it %s", async (_name, open) => {
    // Arrange
    if (!open) pageDoc.documentElement.setAttribute("data-bubble-overlay", "response");
    link?.dispose();
    received = [];
    link = connectToHost({ doc: pageDoc, win: pageWin, presence, webhostFailures, levelWindow, editorIngress, focusLeft, restyleTurns });
    await delivered();
    received = [];

    // Act
    if (open) pageDoc.documentElement.setAttribute("data-bubble-overlay", "response");
    else pageDoc.documentElement.removeAttribute("data-bubble-overlay");
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    expect(received).toContainEqual({ channel: HOST_CHANNEL, kind: "bubbleOverlay", open });
    pageDoc.documentElement.removeAttribute("data-bubble-overlay");
  });
});

describe("Emacs moving its focus", () => {
  it("tells the page's focus-left signal that Emacs moved its focus", () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "focusMoved" });

    // Assert
    expect(leftFor).toEqual([FOCUS_LEFT_EMACS]);
  });
});

describe("digest dismissal records", () => {
  it("records a dismissal at info", async () => {
    // Arrange
    const digest = pageDoc.createElement("div");
    digest.setAttribute("data-component", "news-digest");
    const close = pageDoc.createElement("button");
    close.setAttribute("data-news-digest-close", "");
    digest.append(close);
    pageDoc.body.append(digest);

    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "dismissDigest" });

    // Assert
    await operations();
    expect(capture.sent.find((r) => r.operation === "page.host-dismissed-digest")?.level.case).toBe("info");
  });

  it("records a request with no digest standing at debug", async () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "dismissDigest" });

    // Assert
    await operations();
    expect(capture.sent.find((r) => r.operation === "page.host-dismiss-digest-none")?.level.case).toBe("debug");
  });
});

describe("reported facts", () => {
  it("reports a conversation drawn after it joined", async () => {
    // Act
    pageDoc.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "page");

    // Assert
    await delivered();
    expect(received).toContainEqual({ channel: HOST_CHANNEL, kind: "drawn", what: "page" });
  });

  it("reports a conversation already drawn when it joins", async () => {
    // Arrange
    link?.dispose();
    pageDoc.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "refused");
    received = [];

    // Act
    link = connectToHost({ doc: pageDoc, win: pageWin, presence, webhostFailures, levelWindow, editorIngress, focusLeft, restyleTurns });

    // Assert
    await delivered();
    expect(received).toContainEqual({ channel: HOST_CHANNEL, kind: "drawn", what: "refused" });
  });

  it("reports its selection when it changes", async () => {
    // Act
    pageDoc.dispatchEvent(new Event("selectionchange"));

    // Assert
    await delivered();
    expect(received.some((m) => m.kind === "selection")).toBe(true);
  });

  it("reports a text field taking the keyboard", async () => {
    // Arrange
    const input = pageDoc.createElement("input");
    pageDoc.body.append(input);

    // Act
    input.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));

    // Assert
    await delivered();
    expect(received).toContainEqual({ channel: HOST_CHANNEL, kind: "editing", editing: true });
  });

  it("reports the keyboard leaving its text fields", async () => {
    // Arrange
    const input = pageDoc.createElement("input");
    const button = pageDoc.createElement("button");
    pageDoc.body.append(input, button);
    input.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));

    // Act
    input.dispatchEvent(new FocusEvent("focusout", { bubbles: true, relatedTarget: button }));

    // Assert
    await delivered();
    expect(received).toContainEqual({ channel: HOST_CHANNEL, kind: "editing", editing: false });
  });

  it("reports nothing about the keyboard while it stays out of text fields", async () => {
    // Arrange
    const button = pageDoc.createElement("button");
    pageDoc.body.append(button);

    // Act
    button.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));

    // Assert
    await delivered();
    expect(received.some((m) => m.kind === "editing")).toBe(false);
  });

  it("reports nothing once disposed", async () => {
    // Arrange
    await delivered();
    link?.dispose();
    link = null;
    received = [];

    // Act
    pageDoc.dispatchEvent(new Event("selectionchange"));

    // Assert
    await delivered();
    expect(received).toEqual([]);
  });
});

describe("the webhost's sidebar", () => {
  it("keeps the sidebar column for the webhost's sidebar while it is docked", () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "sidebarDock", docked: true });

    // Assert
    expect(pageDoc.documentElement.classList.contains(WEBHOST_SIDEBAR_CLASS)).toBe(true);
  });

  it("gives the column back when the webhost undocks its sidebar", () => {
    // Arrange
    fromHost({ channel: HOST_CHANNEL, kind: "sidebarDock", docked: true });

    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "sidebarDock", docked: false });

    // Assert
    expect(pageDoc.documentElement.classList.contains(WEBHOST_SIDEBAR_CLASS)).toBe(false);
  });

  it("records the dock at info when it changes", async () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "sidebarDock", docked: true });

    // Assert
    capture.logger.flush();
    await Promise.resolve();
    const record = capture.sent.find((r) => r.operation === "page.webhost-sidebar-dock");
    expect([record?.level.case, record?.context?.["docked"]]).toEqual(["info", true]);
  });

  it("records nothing when the webhost restates the dock", async () => {
    // Arrange
    fromHost({ channel: HOST_CHANNEL, kind: "sidebarDock", docked: true });
    capture.logger.flush();
    await Promise.resolve();
    capture.sent.length = 0;

    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "sidebarDock", docked: true });

    // Assert
    expect(await operations()).not.toContain("page.webhost-sidebar-dock");
  });

  it("lists the webhost's failures for the warning chip", () => {
    // Arrange
    const failure = { arm: "daemonUnreachable", headline: "lost the connection", evidence: [["close", "1006"]] };

    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "webhostFailures", failures: [failure] });

    // Assert
    expect(webhostFailures.standing()).toEqual([failure]);
  });
});

describe("the editor's held-prompt ingress", () => {
  const said = { content: { blocks: [{ text: { text: "sent offline" } }] } };

  it("hands the editor's entries to the held section, decoded", () => {
    // Act
    fromHost({
      channel: HOST_CHANNEL,
      kind: "heldIngress",
      entries: [{ key: "k-1", queuedAt: "2026-10-20T15:18:02.227Z", said }],
    });

    // Assert
    const [entry] = editorIngress.entries();
    expect([entry?.key, entry?.queuedAtMs, entry?.said.content?.blocks[0]?.block.value]).toEqual([
      "k-1",
      Date.parse("2026-10-20T15:18:02.227Z"),
      expect.objectContaining({ text: "sent offline" }),
    ]);
  });

  it("leaves out an entry whose said is no UserSaid, as an error naming its key", async () => {
    // Act
    fromHost({
      channel: HOST_CHANNEL,
      kind: "heldIngress",
      entries: [
        { key: "k-bad", queuedAt: "2026-10-20T15:18:02Z", said: { words: "hi" } },
        { key: "k-1", queuedAt: "2026-10-20T15:18:03Z", said },
      ],
    });

    // Assert
    expect(editorIngress.entries().map((e) => e.key)).toEqual(["k-1"]);
    expect(await operations()).toContain("page.held-ingress-undecodable");
  });
});

describe("the host's log level", () => {
  it("opens the page's window until the end the host names", () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: clock + 60_000 });

    // Assert
    expect([levelWindow.level(), levelWindow.allows("debug").allowed]).toEqual(["debug", true]);
  });

  it("records the window it opened", async () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: clock + 60_000 });

    // Assert
    expect(await operations()).toContain("webapp.log.level-window");
  });

  it("closes the page's window on info", () => {
    // Arrange
    fromHost({ channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: clock + 60_000 });

    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "logLevel", level: "info", untilMs: null });

    // Assert
    expect(levelWindow.level()).toBe("info");
  });

  it("leaves the page at info for a window that already ended", () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: clock - 1 });

    // Assert
    expect(levelWindow.level()).toBe("info");
  });

  it("refuses at error a window longer than a level may last, staying at info", async () => {
    // Act
    fromHost({ channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: clock + 600_000 });

    // Assert
    capture.logger.flush();
    await Promise.resolve();
    const refused = capture.sent.find((record) => record.operation === "webapp.log.level-window" && record.level.case === "error");
    expect([levelWindow.level(), refused !== undefined]).toEqual(["info", true]);
  });
});


describe("the page's log level", () => {
  it("is applied through LevelWindow.apply, never opened or closed by hand", () => {
    // Act
    const shape = levelApplyShapeOf("host/embed.ts");

    // Assert
    expect(shape).toEqual({ applies: true, handRolls: false });
  });
});
