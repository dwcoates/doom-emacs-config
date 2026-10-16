// @vitest-environment jsdom
// The host page: one iframe per workspace, one shown, state replayed on hello,
// facts mirrored for Emacs. See src/host/host.ts.
import { LevelWindow } from "../../../agent-shim/logging/ts/level-window.js";
import { levelApplyShapeOf } from "./level-apply-shape.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  FRAME_SHOWN_ATTRIBUTE,
  FRAME_WORKSPACE_ATTRIBUTE,
  SIDEBAR_BOTTOM_PROPERTY,
  createHost,
  hostHasFocus,
  type Host,
} from "../../src/host/host.js";
import { HOST_CHANNEL, type HostToPage } from "../../src/host/protocol.js";

let host: Host;

beforeEach(() => {
  host = createHost({ doc: document, win: window, level: LevelWindow.fixed("info") });
});

afterEach(() => {
  host.dispose();
  document.body.replaceChildren();
});

function frame(id: string): HTMLIFrameElement {
  const found = document.querySelector<HTMLIFrameElement>(`iframe[${FRAME_WORKSPACE_ATTRIBUTE}="${id}"]`);
  if (found === null) throw new Error(`no frame for ${id}`);
  return found;
}

function parsed(answer: string): Record<string, unknown> {
  return JSON.parse(answer) as Record<string, unknown>;
}

/** Record what the host posts into ID's page. */
function postsTo(id: string): HostToPage[] {
  const posted: HostToPage[] = [];
  const target = frame(id).contentWindow;
  if (target === null) throw new Error(`frame ${id} has no window`);
  vi.spyOn(target, "postMessage").mockImplementation((message: unknown) => {
    posted.push(message as HostToPage);
  });
  return posted;
}

/** Ensure pages for IDS on H. */
function pages(h: Host, ...ids: string[]): void {
  for (const id of ids) h.ensure(id, `http://ws-${id}.localhost:1/`);
}

/** Deliver DATA to the host as though ID's page posted it. */
function fromPage(id: string, data: unknown): void {
  window.dispatchEvent(new MessageEvent("message", { data, source: frame(id).contentWindow }));
}

describe("ensure", () => {
  it("creates an iframe at the URL for a new workspace", () => {
    // Act
    const answer = parsed(host.ensure("a", "http://ws-a.localhost:1/?workspace=a"));

    // Assert
    expect(answer.outcome).toBe("created");
    expect(frame("a").src).toBe("http://ws-a.localhost:1/?workspace=a");
  });

  it("leaves a page alone that is already at the URL", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");

    // Act
    const answer = parsed(host.ensure("a", "http://ws-a.localhost:1/"));

    // Assert
    expect(answer.outcome).toBe("unchanged");
  });

  it("re-points a page at a new URL", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");

    // Act
    const answer = parsed(host.ensure("a", "http://ws-a.localhost:2/"));

    // Assert
    expect(answer.outcome).toBe("navigated");
    expect(frame("a").src).toBe("http://ws-a.localhost:2/");
  });

  it("forgets the drawn mark of a page it re-points", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    fromPage("a", { channel: HOST_CHANNEL, kind: "drawn", what: "page" });

    // Act
    host.ensure("a", "http://ws-a.localhost:2/");

    // Assert
    expect(parsed(host.drawn("a")).drawn).toBeNull();
  });
});

describe("show", () => {
  it("marks the shown page", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");

    // Act
    host.show("a");

    // Assert
    expect(frame("a").hasAttribute(FRAME_SHOWN_ATTRIBUTE)).toBe(true);
  });

  it("unmarks the page shown before", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.ensure("b", "http://ws-b.localhost:1/");
    host.show("a");

    // Act
    const answer = parsed(host.show("b"));

    // Assert
    expect(frame("a").hasAttribute(FRAME_SHOWN_ATTRIBUTE)).toBe(false);
    expect(answer.previous).toBe("a");
  });

  it("tells the newly shown page it is shown", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    const posted = postsTo("a");

    // Act
    host.show("a");

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "presence", shown: true });
  });

  it("tells the previously shown page it is hidden", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.ensure("b", "http://ws-b.localhost:1/");
    host.show("a");
    const posted = postsTo("a");

    // Act
    host.show("b");

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "presence", shown: false });
  });

  it("answers absent for a workspace it holds no page for", () => {
    expect(parsed(host.show("nobody")).outcome).toBe("absent");
  });

  it("answers unchanged for the page already shown", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.show("a");

    // Act / Assert
    expect(parsed(host.show("a")).outcome).toBe("unchanged");
  });

  it("creates a page already shown when its workspace was shown before it existed", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.show("a");
    host.remove("a");
    host.ensure("b", "http://ws-b.localhost:1/");

    // Act
    host.show("b");

    // Assert
    expect(frame("b").hasAttribute(FRAME_SHOWN_ATTRIBUTE)).toBe(true);
  });
});

describe("remove", () => {
  it("drops the workspace's iframe", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");

    // Act
    const answer = parsed(host.remove("a"));

    // Assert
    expect(answer.outcome).toBe("removed");
    expect(document.querySelectorAll("iframe")).toHaveLength(0);
  });

  it("answers absent for a workspace it holds no page for", () => {
    expect(parsed(host.remove("nobody")).outcome).toBe("absent");
  });
});

describe("reload", () => {
  it("loads the page again from its URL", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    frame("a").src = "about:blank";

    // Act
    const answer = parsed(host.reload("a"));

    // Assert
    expect(answer.outcome).toBe("reloaded");
    expect(frame("a").src).toBe("http://ws-a.localhost:1/");
  });
});

describe("commands", () => {
  it("sends the input window's background to the page", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    const posted = postsTo("a");

    // Act
    host.setInputBackground("a", "#101010");

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "inputBackground", background: "#101010" });
  });

  it("sends the docked gate's slot to the page", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    const posted = postsTo("a");

    // Act
    host.setGateDock("a", 140, null);

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "gateDock", pixels: 140, background: null });
  });

  it("asks the page to close its news digest", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    const posted = postsTo("a");

    // Act
    host.dismissDigest("a");

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "dismissDigest" });
  });

  it("answers absent for a command to a workspace it holds no page for", () => {
    expect(parsed(host.setInputBackground("nobody", "#000")).outcome).toBe("absent");
  });
});

describe("hello", () => {
  it("replays the state Emacs gave to a page that says hello", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.setInputBackground("a", "#202020");
    host.setGateDock("a", 90, "#202020");
    host.show("a");
    const posted = postsTo("a");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "hello" });

    // Assert
    expect(posted).toEqual([
      { channel: HOST_CHANNEL, kind: "presence", shown: true },
      { channel: HOST_CHANNEL, kind: "sidebarDock", docked: false },
      { channel: HOST_CHANNEL, kind: "webhostFailures", failures: [] },
      { channel: HOST_CHANNEL, kind: "inputBackground", background: "#202020" },
      { channel: HOST_CHANNEL, kind: "gateDock", pixels: 90, background: "#202020" },
    ]);
  });

  it("forgets the drawn mark of a page that says hello again", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    fromPage("a", { channel: HOST_CHANNEL, kind: "drawn", what: "page" });

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "hello" });

    // Assert
    expect(parsed(host.drawn("a")).drawn).toBeNull();
  });
});

describe("mirrored facts", () => {
  it("answers the drawn mark a page reported", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "drawn", what: "refused" });

    // Assert
    expect(parsed(host.drawn("a"))).toMatchObject({ outcome: "present", drawn: "refused" });
  });

  it("answers the shown page's selection", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.show("a");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "selection", text: "picked", focused: true });

    // Assert
    expect(parsed(host.selection())).toMatchObject({ id: "a", text: "picked", focused: true });
  });

  it("answers no selection when no page is shown", () => {
    expect(parsed(host.selection())).toMatchObject({ outcome: "none-shown", text: null });
  });

  it("answers whether a text field of the shown page holds the keyboard", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.show("a");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "editing", editing: true });

    // Assert
    expect(host.editing()).toBe(true);
  });

  it("does not answer for a page that is not shown", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.ensure("b", "http://ws-b.localhost:1/");
    host.show("b");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "editing", editing: true });

    // Assert
    expect(host.editing()).toBe(false);
  });
});

describe("notes", () => {
  it("notes a message from another build at info and hands the note to the next answer", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    fromPage("a", { channel: HOST_CHANNEL, kind: "wave" });

    // Act
    const answer = parsed(host.ids());

    // Assert
    expect(answer.notes).toEqual([{ level: "info", text: expect.stringContaining("cannot read") as unknown }]);
  });

  it("notes a message on its channel from no page it holds", () => {
    // Arrange
    window.dispatchEvent(new MessageEvent("message", { data: { channel: HOST_CHANNEL, kind: "hello" }, source: window }));

    // Act
    const answer = parsed(host.ids());

    // Assert
    expect(answer.notes).toEqual([{ level: "warn", text: expect.stringContaining("no workspace page") as unknown }]);
  });

  it("hands each note to one answer only", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    fromPage("a", { channel: HOST_CHANNEL, kind: "wave" });
    host.ids();

    // Act
    const answer = parsed(host.ids());

    // Assert
    expect(answer.notes).toEqual([]);
  });

  it("ignores a message on another channel", () => {
    // Arrange
    window.dispatchEvent(new MessageEvent("message", { data: { channel: "vite" }, source: window }));

    // Act
    const answer = parsed(host.ids());

    // Assert
    expect(answer.notes).toEqual([]);
  });
});

describe("hostHasFocus", () => {
  it("answers no when nothing holds the focus", () => {
    // Arrange
    const doc = { activeElement: null } as unknown as Document;

    // Act / Assert
    expect(hostHasFocus(doc, host)).toBe(false);
  });

  it("answers for the shown page when an iframe holds the focus", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.show("a");
    fromPage("a", { channel: HOST_CHANNEL, kind: "editing", editing: true });
    const doc = { activeElement: frame("a") } as unknown as Document;

    // Act / Assert
    expect(hostHasFocus(doc, host)).toBe(true);
  });

  it("answers yes for a text field of the host itself", () => {
    // Arrange
    const doc = { activeElement: document.createElement("textarea") } as unknown as Document;

    // Act / Assert
    expect(hostHasFocus(doc, host)).toBe(true);
  });

  it("answers no for any other focused element", () => {
    // Arrange
    const doc = { activeElement: document.createElement("button") } as unknown as Document;

    // Act / Assert
    expect(hostHasFocus(doc, host)).toBe(false);
  });
});

describe("the webhost's sidebar", () => {
  /** A failure row as the webhost relays it. */
  const FAILURE = { arm: "daemonUnreachable", headline: "lost the connection", evidence: [["close", "1006"]] } as const;

  /** ID's page reports its gate band PIXELS tall. */
  function band(id: string, pixels: number): void {
    fromPage(id, { channel: HOST_CHANNEL, kind: "gateBand", pixels });
  }

  /** Where the sidebar ends, as the webhost set it. */
  function sidebarBottom(): string {
    return document.documentElement.style.getPropertyValue(SIDEBAR_BOTTOM_PROPERTY);
  }

  afterEach(() => {
    document.documentElement.style.removeProperty(SIDEBAR_BOTTOM_PROPERTY);
  });

  it("tells every page when it docks the sidebar over it", () => {
    // Arrange
    pages(host, "a", "b");
    const toA = postsTo("a");
    const toB = postsTo("b");

    // Act
    host.setSidebarDocked(true);

    // Assert
    const dock = { channel: HOST_CHANNEL, kind: "sidebarDock", docked: true };
    expect([toA, toB]).toEqual([[dock], [dock]]);
  });

  it("tells a page that says hello after the dock that the sidebar is docked", () => {
    // Arrange
    pages(host, "a");
    host.setSidebarDocked(true);
    const posted = postsTo("a");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "hello" });

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "sidebarDock", docked: true });
  });

  it("tells every page its own standing failures", () => {
    // Arrange
    pages(host, "a", "b");
    const toB = postsTo("b");

    // Act
    host.setFailures([FAILURE]);

    // Assert
    expect(toB).toEqual([{ channel: HOST_CHANNEL, kind: "webhostFailures", failures: [FAILURE] }]);
  });

  it("tells a page that says hello the failures standing", () => {
    // Arrange
    pages(host, "a");
    host.setFailures([FAILURE]);
    const posted = postsTo("a");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "hello" });

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "webhostFailures", failures: [FAILURE] });
  });

  it("ends the sidebar above the gate band of the page on screen", () => {
    // Arrange
    pages(host, "a");
    host.show("a");

    // Act
    band("a", 120);

    // Assert
    expect(sidebarBottom()).toBe("120px");
  });

  it("ignores the gate band of a page that is not on screen", () => {
    // Arrange
    pages(host, "a", "b");
    host.show("a");
    band("a", 0);

    // Act
    band("b", 120);

    // Assert
    expect(sidebarBottom()).toBe("0px");
  });

  it("moves the sidebar's end to the band of the page shown next", () => {
    // Arrange
    pages(host, "a", "b");
    host.show("a");
    band("b", 90);

    // Act
    host.show("b");

    // Assert
    expect(sidebarBottom()).toBe("90px");
  });

  it("runs the sidebar to the bottom once the page on screen is removed", () => {
    // Arrange
    pages(host, "a");
    host.show("a");
    band("a", 120);

    // Act
    host.remove("a");

    // Assert
    expect(sidebarBottom()).toBe("0px");
  });

  it("answers text selected in the webhost itself as the selection", () => {
    // Arrange
    pages(host, "a");
    host.show("a");
    const text = document.createElement("p");
    text.textContent = "a sidebar row";
    document.body.append(text);
    window.getSelection()?.selectAllChildren(text);

    // Act
    const answer = parsed(host.selection());

    // Assert
    expect([answer.outcome, answer.text]).toEqual(["webhost", "a sidebar row"]);
    window.getSelection()?.removeAllRanges();
  });

  it("holds the workspace pages in the element it is given", () => {
    // Arrange
    host.dispose();
    const stack = document.createElement("div");
    document.body.append(stack);
    host = createHost({ doc: document, win: window, level: LevelWindow.fixed("info"), frames: stack });

    // Act
    host.ensure("a", "http://ws-a.localhost:1/");

    // Assert
    expect(stack.querySelector("iframe")).toBe(frame("a"));
  });
});

describe("setLogLevel", () => {
  /** A host on CLOCK, holding the level window it raises. */
  function leveled(clock: { ms: number }) {
    host.dispose();
    const level = new LevelWindow({ level: "info", untilMs: null }, () => clock.ms);
    host = createHost({ doc: document, win: window, level, now: () => clock.ms });
    return level;
  }

  it("raises the webhost's own level until the end it names", () => {
    // Arrange
    const clock = { ms: 1_000_000 };
    const level = leveled(clock);

    // Act
    const answer = parsed(host.setLogLevel("debug", 1_060_000));

    // Assert
    expect([answer.outcome, level.level()]).toEqual(["opened", "debug"]);
  });

  it("tells every page the window", () => {
    // Arrange
    leveled({ ms: 1_000_000 });
    pages(host, "a");
    const posted = postsTo("a");

    // Act
    host.setLogLevel("debug", 1_060_000);

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: 1_060_000 });
  });

  it("replays a standing window to a page that says hello", () => {
    // Arrange
    leveled({ ms: 1_000_000 });
    pages(host, "a");
    host.setLogLevel("debug", 1_060_000);
    const posted = postsTo("a");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "hello" });

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: 1_060_000 });
  });

  it("replays no window that has ended", () => {
    // Arrange
    const clock = { ms: 1_000_000 };
    leveled(clock);
    pages(host, "a");
    host.setLogLevel("debug", 1_060_000);
    clock.ms = 1_060_000;
    const posted = postsTo("a");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "hello" });

    // Assert
    expect(posted.some((message) => message.kind === "logLevel")).toBe(false);
  });

  it("closes the window everywhere on info", () => {
    // Arrange
    const level = leveled({ ms: 1_000_000 });
    pages(host, "a");
    host.setLogLevel("debug", 1_060_000);
    const posted = postsTo("a");

    // Act
    const answer = parsed(host.setLogLevel("info", null));

    // Assert
    expect([answer.outcome, level.level(), posted]).toEqual([
      "closed",
      "info",
      [{ channel: HOST_CHANNEL, kind: "logLevel", level: "info", untilMs: null }],
    ]);
  });

  it.each([
    ["an unknown level", "trace", 1_060_000],
    ["a window with no end", "debug", null],
    ["an info level with an end", "info", 1_060_000],
    ["a window longer than a level may last", "debug", 1_000_000 + 600_000],
  ] as const)("refuses %s, leaving the level at info", (_name, name, untilMs) => {
    // Arrange
    const level = leveled({ ms: 1_000_000 });

    // Act
    const answer = parsed(host.setLogLevel(name, untilMs));

    // Assert
    expect([answer.outcome, level.level(), (answer.notes as unknown[]).length]).toEqual(["refused", "info", 1]);
  });
});


describe("the webhost's log level", () => {
  it("is applied through LevelWindow.apply, never opened or closed by hand", () => {
    // Act
    const shape = levelApplyShapeOf("host/host.ts");

    // Assert
    expect(shape).toEqual({ applies: true, handRolls: false });
  });
});
