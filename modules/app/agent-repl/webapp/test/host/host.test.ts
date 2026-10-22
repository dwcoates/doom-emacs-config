// @vitest-environment jsdom
// The host page: one iframe per workspace, one shown, state replayed on hello,
// facts mirrored for Emacs. See src/host/host.ts.
import { LevelWindow } from "../../../agent-shim/logging/ts/level-window.js";
import { levelApplyShapeOf } from "./level-apply-shape.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  IN_TURN_BOUND_MS,
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

/** Ensure ID's page in turn in LANE on H, answering the parsed answer. */
function inTurn(h: Host, id: string, lane = "right"): Record<string, unknown> {
  return parsed(h.ensure(id, `http://ws-${id}.localhost:1/`, lane));
}

/** Whether ID's page has started loading. */
const loading = (id: string): boolean => frame(id).getAttribute("src") !== null;

describe("onShown", () => {
  it("tells a listener every page shown, and nothing for the page already shown", () => {
    // Arrange
    pages(host, "a", "b");
    const told: string[] = [];
    host.onShown((id) => told.push(id));

    // Act
    host.show("a");
    host.show("a");
    host.show("b");

    // Assert
    expect(told).toEqual(["a", "b"]);
  });

  it("tells nothing for a page it does not hold", () => {
    // Arrange
    const told: string[] = [];
    host.onShown((id) => told.push(id));

    // Act
    host.show("absent");

    // Assert
    expect(told).toEqual([]);
  });

  it("tells a stopped listener nothing", () => {
    // Arrange
    pages(host, "a");
    const told: string[] = [];
    const stopListening = host.onShown((id) => told.push(id));
    stopListening();

    // Act
    host.show("a");

    // Assert
    expect(told).toEqual([]);
  });
});

describe("ensure in turn", () => {
  it("loads the first page in turn at once", () => {
    // Act
    const answer = inTurn(host, "a");

    // Assert
    expect([answer.outcome, loading("a")]).toEqual(["created", true]);
  });

  it("holds a later page until the one loading before it has drawn", () => {
    // Arrange
    inTurn(host, "a");

    // Act
    const answer = inTurn(host, "b");

    // Assert
    expect([answer.outcome, loading("b")]).toEqual(["queued", false]);
  });

  it.each(["page", "refused"])("starts the next page once the one before says it drew (%s)", (what) => {
    // Arrange
    inTurn(host, "a");
    inTurn(host, "b");
    inTurn(host, "c");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "drawn", what });

    // Assert
    expect([loading("b"), loading("c")]).toEqual([true, false]);
  });

  it("loads a page still waiting its turn the moment it is shown", () => {
    // Arrange
    inTurn(host, "a");
    inTurn(host, "b");

    // Act
    const answer = parsed(host.show("b"));

    // Assert
    expect([answer.jumped, loading("b")]).toEqual([true, true]);
  });

  it("never loads a page removed while it waited", () => {
    // Arrange
    inTurn(host, "a");
    inTurn(host, "b");
    inTurn(host, "c");
    host.remove("b");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "drawn", what: "page" });

    // Assert
    expect(loading("c")).toBe(true);
  });

  it("starts the next page when the one loading in turn is removed", () => {
    // Arrange
    inTurn(host, "a");
    inTurn(host, "b");

    // Act
    host.remove("a");

    // Assert
    expect(loading("b")).toBe(true);
  });

  it("loads the first page of each lane at once, side by side", () => {
    // Act
    inTurn(host, "r1", "right");
    inTurn(host, "l1", "left");

    // Assert
    expect([loading("r1"), loading("l1")]).toEqual([true, true]);
  });

  it("holds a lane's later page behind its own lane only", () => {
    // Arrange
    inTurn(host, "r1", "right");
    inTurn(host, "r2", "right");
    inTurn(host, "l1", "left");
    inTurn(host, "l2", "left");

    // Act
    fromPage("l1", { channel: HOST_CHANNEL, kind: "drawn", what: "page" });

    // Assert
    expect([loading("r2"), loading("l2")]).toEqual([false, true]);
  });

  it("loads a page waiting in either lane the moment it is shown", () => {
    // Arrange
    inTurn(host, "r1", "right");
    inTurn(host, "l1", "left");
    inTurn(host, "l2", "left");

    // Act
    const answer = parsed(host.show("l2"));

    // Assert
    expect([answer.jumped, loading("l2")]).toEqual([true, true]);
  });

  it("loads a page ensured plainly at once, whatever is loading in turn", () => {
    // Arrange
    inTurn(host, "a");

    // Act
    host.ensure("z", "http://ws-z.localhost:1/");

    // Assert
    expect(loading("z")).toBe(true);
  });

  it("stops every lane's bound on dispose", () => {
    // Arrange
    vi.useFakeTimers();
    try {
      const timed = createHost({ doc: document, win: window, level: LevelWindow.fixed("info") });
      inTurn(timed, "p", "right");
      inTurn(timed, "q", "left");

      // Act
      timed.dispose();

      // Assert
      expect(vi.getTimerCount()).toBe(0);
    } finally {
      vi.useRealTimers();
    }
  });

  it("starts the next page, recorded at warn, when one never says it drew", () => {
    // Arrange
    vi.useFakeTimers();
    try {
      const timed = createHost({ doc: document, win: window, level: LevelWindow.fixed("info") });
      try {
        inTurn(timed, "p");
        inTurn(timed, "q");

        // Act
        vi.advanceTimersByTime(IN_TURN_BOUND_MS);

        // Assert
        const notes = parsed(timed.ids()).notes as { level: string; text: string }[];
        expect([loading("q"), notes.some((n) => n.level === "warn" && n.text.includes("page p loading in turn in lane right"))]).toEqual([
          true,
          true,
        ]);
      } finally {
        timed.dispose();
      }
    } finally {
      vi.useRealTimers();
    }
  });
});

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

  it("re-points a page into a fresh frame, so the old document cannot run on", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    const old = frame("a");

    // Act
    host.ensure("a", "http://ws-a.localhost:2/");

    // Assert
    expect([old.isConnected, frame("a") === old]).toEqual([false, false]);
  });

  it("keeps the desired state Emacs gave across a re-point", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.setInputBackground("a", "#123456");
    host.setGateDock("a", 40, "#abcdef");

    // Act
    host.ensure("a", "http://ws-a.localhost:2/");
    const posted = postsTo("a");
    fromPage("a", { channel: HOST_CHANNEL, kind: "hello" });

    // Assert
    expect(posted).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ kind: "inputBackground", background: "#123456" }),
        expect.objectContaining({ kind: "gateDock", pixels: 40, background: "#abcdef" }),
      ]),
    );
  });

  it("keeps a shown page shown across a re-point", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    host.show("a");

    // Act
    host.ensure("a", "http://ws-a.localhost:2/");

    // Assert
    expect(frame("a").hasAttribute(FRAME_SHOWN_ATTRIBUTE)).toBe(true);
  });

  it("loads a page re-pointed while it waited in a lane at once, and only once", () => {
    // Arrange
    inTurn(host, "p");
    inTurn(host, "q");

    // Act
    const answer = parsed(host.ensure("q", "http://ws-q.localhost:2/", "right"));
    const repointed = frame("q");
    fromPage("p", { channel: HOST_CHANNEL, kind: "drawn", what: "page" });

    // Assert
    expect([answer.outcome, repointed.getAttribute("src"), frame("q") === repointed]).toEqual([
      "navigated",
      "http://ws-q.localhost:2/",
      true,
    ]);
  });

  it("lets a lane go on when the page it was loading is re-pointed", () => {
    // Arrange
    inTurn(host, "p");
    inTurn(host, "q");

    // Act
    host.ensure("p", "http://ws-p.localhost:2/");

    // Assert
    expect(loading("q")).toBe(true);
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

  it("sends the editor's held-prompt ingress entries to the page", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    const posted = postsTo("a");
    const entries = [{ key: "k-1", queuedAt: "2026-10-20T15:18:02Z", said: { content: {} } }];

    // Act
    const outcome = parsed(host.setHeldIngress("a", entries)).outcome;

    // Assert
    expect([outcome, posted]).toEqual(["sent", [{ channel: HOST_CHANNEL, kind: "heldIngress", entries }]]);
  });

  it("refuses ingress entries that are not ingress entries, posting nothing", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    const posted = postsTo("a");

    // Act
    const answer = parsed(host.setHeldIngress("a", [{ key: 3 }]));

    // Assert
    expect([answer.outcome, posted, (answer.notes as unknown[]).length]).toEqual(["refused", [], 1]);
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

  it("replays the editor's held-prompt ingress entries to a page that says hello", () => {
    // Arrange
    host.ensure("a", "http://ws-a.localhost:1/");
    const entries = [{ key: "k-1", queuedAt: "2026-10-20T15:18:02Z", said: { content: {} } }];
    host.setHeldIngress("a", entries);
    const posted = postsTo("a");

    // Act
    fromPage("a", { channel: HOST_CHANNEL, kind: "hello" });

    // Assert
    expect(posted).toContainEqual({ channel: HOST_CHANNEL, kind: "heldIngress", entries });
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

describe("a hidden page keeps the size it was hidden at", () => {
  const made: Host[] = [];
  afterEach(() => {
    for (const each of made.splice(0)) each.dispose();
  });

  /** A host whose frames measure at whatever SIZE holds, recording that each was laid out when measured. */
  function sizedHost(size: { width: number; height: number }): { host: Host; detached: string[] } {
    const detached: string[] = [];
    const sized = createHost({
      doc: document,
      win: window,
      level: LevelWindow.fixed("info"),
      measure: (element) => {
        if (!element.isConnected) detached.push(element.getAttribute(FRAME_WORKSPACE_ATTRIBUTE) ?? "?");
        return { ...size };
      },
    });
    made.push(sized);
    return { host: sized, detached };
  }
  const frameOf = (id: string): HTMLIFrameElement =>
    document.querySelector<HTMLIFrameElement>(`iframe[${FRAME_WORKSPACE_ATTRIBUTE}="${id}"]`)!;

  it("fixes a frame created hidden at the size it fills, measured once it is in the document", () => {
    // Arrange
    const { host: sized, detached } = sizedHost({ width: 800, height: 600 });

    // Act
    sized.ensure("a", "http://a.example/");

    // Assert
    expect([frameOf("a").style.width, frameOf("a").style.height, detached]).toEqual(["800px", "600px", []]);
  });

  it("lets the shown frame fill the view, and fixes the one it replaced at its size then", () => {
    // Arrange
    const size = { width: 800, height: 600 };
    const { host: sized } = sizedHost(size);
    sized.ensure("a", "http://a.example/");
    sized.ensure("b", "http://b.example/");
    sized.show("a");
    size.height = 700;

    // Act
    sized.show("b");

    // Assert
    expect([frameOf("b").style.width, frameOf("b").style.height]).toEqual(["", ""]);
    expect([frameOf("a").style.width, frameOf("a").style.height]).toEqual(["800px", "700px"]);
  });

  it("lets a frame shown again fill the view at whatever size it is now", () => {
    // Arrange
    const { host: sized } = sizedHost({ width: 800, height: 600 });
    sized.ensure("a", "http://a.example/");
    sized.ensure("b", "http://b.example/");
    sized.show("a");
    sized.show("b");

    // Act
    sized.show("a");

    // Assert
    expect([frameOf("a").style.width, frameOf("a").style.height]).toEqual(["", ""]);
  });
});

describe("show with a switch's start", () => {
  const hello = { channel: HOST_CHANNEL, kind: "hello" } as const;
  const presenceIn = (posted: HostToPage[]) => posted.filter((m) => m.kind === "presence");

  it("tells a listening page when the switch that showed it began", () => {
    // Arrange
    pages(host, "a");
    fromPage("a", hello);
    const posted = postsTo("a");

    // Act
    host.show("a", 1_700_000_000_123);

    // Assert
    expect(presenceIn(posted)).toEqual([
      { channel: HOST_CHANNEL, kind: "presence", shown: true, switchStartedAtMs: 1_700_000_000_123 },
    ]);
  });

  it("hands a page still loading its switch with the presence its hello replays, once", () => {
    // Arrange
    pages(host, "a");
    host.show("a", 1_700_000_000_123);
    const posted = postsTo("a");

    // Act
    fromPage("a", hello);
    fromPage("a", hello);

    // Assert
    expect(presenceIn(posted)).toEqual([
      { channel: HOST_CHANNEL, kind: "presence", shown: true, switchStartedAtMs: 1_700_000_000_123 },
      { channel: HOST_CHANNEL, kind: "presence", shown: true },
    ]);
  });

  it("forgets the switch of a page reloaded before it said hello", () => {
    // Arrange
    pages(host, "a");
    host.show("a", 1_700_000_000_123);
    host.reload("a");
    const posted = postsTo("a");

    // Act
    fromPage("a", hello);

    // Assert
    expect(presenceIn(posted)).toEqual([{ channel: HOST_CHANNEL, kind: "presence", shown: true }]);
  });

  it("forgets the switch of a page hidden again before it said hello", () => {
    // Arrange
    pages(host, "a", "b");
    host.show("a", 1_700_000_000_123);
    host.show("b");
    host.show("a");
    const posted = postsTo("a");

    // Act
    fromPage("a", hello);

    // Assert
    expect(presenceIn(posted)).toEqual([{ channel: HOST_CHANNEL, kind: "presence", shown: true }]);
  });
});
