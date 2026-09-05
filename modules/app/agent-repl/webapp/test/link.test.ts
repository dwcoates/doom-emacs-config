// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  OpenExternalErrorSchema,
  OpenExternalResponseSchema,
  type OpenExternalRequest,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import {
  OpenInEditorErrorSchema,
  OpenInEditorResponseSchema,
  type OpenInEditorRequest,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import { oneofArms } from "./arms.js";
import { WorkspaceRefSchema } from "../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../src/clock.js";
import type { FailureSink } from "../src/failure/sink.js";
import { ForwardingLogger, setLogger } from "../src/log.js";
import {
  openExternalRefusal,
  openInEditorRefusal,
  renderEditorLink,
  renderExternalLink,
} from "../src/link.js";
import { createAgentReplClient } from "../src/rpc/client.js";
import { MalformedView } from "../src/rpc/malformed.js";
import { createAppContext, type AppContext } from "../src/rpc/context.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/home/u/w" });
const SINK: FailureSink = { report: () => {}, retract: () => {} };

interface Harness {
  ctx: AppContext;
  external: OpenExternalRequest[];
  editor: OpenInEditorRequest[];
}

/** What each fact-carrying arm must carry for its sentence to be complete. */
const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  workspaceRefMismatch: { registryDir: "/w/registry" },
  transferringAway: { address: "127.0.0.1:7777" },
  launchFailed: { detail: "no such file" },
};


/**
 * The refusal a click left at the link.
 *
 * It is the shared `.refusal[data-arm]` ELEMENT the one refusal hook draws,
 * not a class on the anchor: the sentence carries the arm's own facts (a
 * registry dir, a successor's address) and an anchor whose text is a url has
 * nowhere to put them. A detached anchor hosts it inside itself; in a page it
 * lands as the anchor's next sibling.
 */
function refusalAt(anchor: HTMLElement): HTMLElement | null {
  return anchor.querySelector<HTMLElement>(".refusal");
}

/**
 * A context whose two link verbs answer ARM and record their requests.
 *
 * "error" refuses with CAUSE, since landing 4 gave every error a typed cause
 * and an unset one is a malformed view rather than a drawable refusal.
 */
function harness(
  arm: "success" | "error" | "throw" = "success",
  cause = "invalidUrl",
  editorCause = "pathEscapesWorkspace",
): Harness {
  const external: OpenExternalRequest[] = [];
  const editor: OpenInEditorRequest[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openExternal: (req) => {
        external.push(req);
        if (arm === "throw") throw new ConnectError("gone", Code.Unavailable);
        return create(OpenExternalResponseSchema, {
          result:
            arm === "error"
              ? { case: "error", value: { cause: { case: cause, value: CAUSE_FILL[cause] ?? {} } } }
              : { case: "success", value: {} },
        } as never);
      },
      openInEditor: (req) => {
        editor.push(req);
        if (arm === "throw") throw new ConnectError("gone", Code.Unavailable);
        return create(OpenInEditorResponseSchema, {
          result:
            arm === "error"
              ? {
                  case: "error",
                  value: { cause: { case: editorCause, value: CAUSE_FILL[editorCause] ?? {} } },
                }
              : { case: "success", value: {} },
        } as never);
      },
    });
  });
  const ctx = createAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: createTicker(1000),
    failures: SINK,
    composerEnabled: false,
  });
  return { ctx, external, editor };
}

/** A primary, unmodified click, as a user makes. */
function click(el: HTMLElement): MouseEvent {
  const event = new MouseEvent("click", { bubbles: true, cancelable: true, button: 0 });
  el.dispatchEvent(event);
  return event;
}

/**
 * Let the click's rpc settle.
 *
 * The router transport hands the answer back through a zero-delay timer, so a
 * microtask drain alone never reaches the refusal the handler draws.
 * Advancing by 0 runs those without moving the clock.
 */
async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

beforeEach(() => {
  vi.useFakeTimers();
  // A click this component deliberately does NOT claim (a meta-click, an
  // unlinkable scheme) reaches jsdom's own anchor handling, which has no
  // navigation to perform and says so on stderr. Production runs inside a
  // webview where the same click is the platform's to handle; here the page
  // stands in for it, so nothing is left for jsdom to warn about.
  window.addEventListener("click", swallowNavigation);
});

afterEach(() => {
  window.removeEventListener("click", swallowNavigation);
  vi.useRealTimers();
});

function swallowNavigation(event: Event): void {
  event.preventDefault();
}

describe("renderExternalLink: what it renders", () => {
  it("renders an anchor for an https url", () => {
    const { ctx } = harness();
    expect(renderExternalLink(ctx, { text: "docs", url: "https://example.test" }).tagName).toBe("A");
  });

  it("renders an anchor for an http url", () => {
    const { ctx } = harness();
    expect(renderExternalLink(ctx, { text: "docs", url: "http://example.test" }).tagName).toBe("A");
  });

  it("keeps the href, so the destination shows on hover", () => {
    const { ctx } = harness();
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test/x" });
    expect(a.getAttribute("href")).toBe("https://example.test/x");
  });

  it("draws the given text", () => {
    const { ctx } = harness();
    expect(renderExternalLink(ctx, { text: "docs", url: "https://example.test" }).textContent).toBe("docs");
  });

  it("falls back to the url when the text is empty, so no link is blank", () => {
    const { ctx } = harness();
    expect(renderExternalLink(ctx, { text: "", url: "https://example.test" }).textContent).toBe(
      "https://example.test",
    );
  });

  const unlinkable = ["mailto:a@b.test", "javascript:alert(1)", "file:///etc/passwd", "/plain/path", "ftp://h/x"];
  for (const url of unlinkable) {
    it(`renders ${url.split(":")[0]} as plain text rather than a link`, () => {
      const { ctx } = harness();
      expect(renderExternalLink(ctx, { text: "x", url }).tagName).toBe("SPAN");
    });
  }

  it("warns when it refuses to link a destination", () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => {}, (level, line) => lines.push([level, line])));
    const { ctx } = harness();
    renderExternalLink(ctx, { text: "x", url: "mailto:a@b.test" });
    expect(lines.some(([level, line]) => level === "warn" && line.includes("link.unlinkable-scheme"))).toBe(true);
  });
});

describe("renderExternalLink: the click", () => {
  it("cancels the click, so the webview never navigates", () => {
    const { ctx } = harness();
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    expect(click(a).defaultPrevented).toBe(true);
  });

  it("calls OpenExternal", async () => {
    const { ctx, external } = harness();
    click(renderExternalLink(ctx, { text: "docs", url: "https://example.test" }));
    await settle();
    expect(external).toHaveLength(1);
  });

  it("sends the url verbatim", async () => {
    const { ctx, external } = harness();
    click(renderExternalLink(ctx, { text: "docs", url: "https://example.test/a?b=c#d" }));
    await settle();
    expect(external[0].url).toBe("https://example.test/a?b=c#d");
  });

  it("addresses the request to this page's workspace", async () => {
    const { ctx, external } = harness();
    click(renderExternalLink(ctx, { text: "docs", url: "https://example.test" }));
    await settle();
    expect(external[0].workspace?.id).toBe("ws-1");
  });

  it("draws NOTHING on success, since the result happens elsewhere", async () => {
    const { ctx } = harness("success");
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    click(a);
    await settle();
    expect(refusalAt(a)).toBeNull();
  });

  it("draws the refusal AT THE LINK on an error arm", async () => {
    // ARRANGE
    const { ctx } = harness("error");
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    // ACT
    click(a);
    await settle();
    // ASSERT
    expect(refusalAt(a)?.getAttribute("data-arm")).toBe("invalidUrl");
  });

  it.each(oneofArms(OpenExternalErrorSchema, "cause"))(
    "labels the %s arm and says something about it",
    async (arm) => {
      const { ctx } = harness("error", arm);
      const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
      click(a);
      await settle();
      const refusal = refusalAt(a);
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent === ""]).toEqual([
        arm,
        false,
      ]);
    },
  );

  it("names the successor daemon on a transfer, from the one shared wording", async () => {
    const { ctx } = harness("error", "transferringAway");
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    click(a);
    await settle();
    expect(refusalAt(a)?.textContent).toContain("127.0.0.1:7777");
  });

  it("carries the launcher's own detail", async () => {
    const { ctx } = harness("error", "launchFailed");
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    click(a);
    await settle();
    expect(refusalAt(a)?.textContent).toContain("no such file");
  });

  it("marks the refusal with the shared class the suite targets", async () => {
    const { ctx } = harness("error");
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    click(a);
    await settle();
    expect(refusalAt(a)).not.toBeNull();
  });

  it("draws the refusal's sentence as its text", async () => {
    const { ctx } = harness("error");
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    click(a);
    await settle();
    expect(refusalAt(a)?.textContent).not.toBe("");
  });

  it("warns on a refusal", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => {}, (level, line) => lines.push([level, line])));
    const { ctx } = harness("error");
    click(renderExternalLink(ctx, { text: "docs", url: "https://example.test" }));
    await settle();
    expect(lines.some(([level, line]) => level === "warn" && line.includes("link.open-external-refused"))).toBe(true);
  });

  it("says so when the daemon could not be reached at all", async () => {
    const { ctx } = harness("throw");
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    click(a);
    await settle();
    expect(refusalAt(a)?.getAttribute("data-arm")).toBe("transport");
  });

  it("clears a previous refusal when the link is clicked again", async () => {
    // ARRANGE: the daemon refused once; a retry must not look pre-failed.
    const { ctx, external } = harness("success");
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    const stale = document.createElement("span");
    stale.className = "refusal";
    stale.setAttribute("data-arm", "invalidUrl");
    a.appendChild(stale);
    // ACT
    click(a);
    await settle();
    // ASSERT
    expect([refusalAt(a) !== null, external.length]).toEqual([false, 1]);
  });

  const ignored: ReadonlyArray<[string, MouseEventInit]> = [
    ["a meta-click", { metaKey: true }],
    ["a ctrl-click", { ctrlKey: true }],
    ["a shift-click", { shiftKey: true }],
    ["an alt-click", { altKey: true }],
    ["a middle click", { button: 1 }],
  ];
  for (const [label, init] of ignored) {
    it(`leaves ${label} to the platform`, async () => {
      const { ctx, external } = harness();
      const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
      // In the document, so the click reaches the page-level listener that
      // stands in for the webview -- an unclaimed click is the platform's.
      document.body.append(a);
      a.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, ...init }));
      await settle();
      expect(external).toHaveLength(0);
    });
  }

  it("leaves an already-cancelled click to whoever claimed it", async () => {
    const { ctx, external } = harness();
    const a = renderExternalLink(ctx, { text: "docs", url: "https://example.test" });
    const event = new MouseEvent("click", { bubbles: true, cancelable: true, button: 0 });
    event.preventDefault();
    a.dispatchEvent(event);
    await settle();
    expect(external).toHaveLength(0);
  });
});

describe("renderEditorLink", () => {
  it("renders an anchor", () => {
    const { ctx } = harness();
    expect(renderEditorLink(ctx, { text: "plan.md", path: "/w/plan.md" }).tagName).toBe("A");
  });

  it("carries the shared editor-link hook, whichever view drew it", () => {
    const { ctx } = harness();
    const a = renderEditorLink(ctx, { text: "plan.md", path: "/w/plan.md" });
    expect(a.hasAttribute("data-editor-link")).toBe(true);
  });

  it("carries NO href, since the destination is on the daemon's host", () => {
    const { ctx } = harness();
    expect(renderEditorLink(ctx, { text: "plan.md", path: "/w/plan.md" }).hasAttribute("href")).toBe(false);
  });

  it("marks the path for the integration suite", () => {
    const { ctx } = harness();
    const a = renderEditorLink(ctx, { text: "plan.md", path: "/w/plan.md" });
    expect(a.getAttribute("data-host-path")).toBe("/w/plan.md");
  });

  it("marks the line when one was given", () => {
    const { ctx } = harness();
    const a = renderEditorLink(ctx, { text: "plan.md", path: "/w/plan.md", line: 42 });
    expect(a.getAttribute("data-host-line")).toBe("42");
  });

  it("omits the line marker when none was given", () => {
    const { ctx } = harness();
    const a = renderEditorLink(ctx, { text: "plan.md", path: "/w/plan.md" });
    expect(a.hasAttribute("data-host-line")).toBe(false);
  });

  it("falls back to the path when the text is empty", () => {
    const { ctx } = harness();
    expect(renderEditorLink(ctx, { text: "", path: "/w/plan.md" }).textContent).toBe("/w/plan.md");
  });

  it("is reachable by keyboard", () => {
    const { ctx } = harness();
    expect(renderEditorLink(ctx, { text: "x", path: "/w/x" }).tabIndex).toBe(0);
  });

  it("cancels the click", () => {
    const { ctx } = harness();
    expect(click(renderEditorLink(ctx, { text: "x", path: "/w/x" })).defaultPrevented).toBe(true);
  });

  it("calls OpenInEditor", async () => {
    const { ctx, editor } = harness();
    click(renderEditorLink(ctx, { text: "x", path: "/w/x" }));
    await settle();
    expect(editor).toHaveLength(1);
  });

  it("relays the path verbatim", async () => {
    const { ctx, editor } = harness();
    click(renderEditorLink(ctx, { text: "x", path: "/w/a b/c.md" }));
    await settle();
    expect(editor[0].path).toBe("/w/a b/c.md");
  });

  it("sends the line when one was given", async () => {
    const { ctx, editor } = harness();
    click(renderEditorLink(ctx, { text: "x", path: "/w/x", line: 42 }));
    await settle();
    expect(editor[0].line).toBe(42);
  });

  it("leaves the line UNSET when none was given, rather than sending a zero", async () => {
    // ARRANGE: unset means the file's top; a zero would claim line zero exists.
    const { ctx, editor } = harness();
    // ACT
    click(renderEditorLink(ctx, { text: "x", path: "/w/x" }));
    await settle();
    // ASSERT
    expect(editor[0].line).toBeUndefined();
  });

  it("addresses the request to this page's workspace", async () => {
    const { ctx, editor } = harness();
    click(renderEditorLink(ctx, { text: "x", path: "/w/x" }));
    await settle();
    expect(editor[0].workspace?.id).toBe("ws-1");
  });

  it("draws nothing on success", async () => {
    const { ctx } = harness("success");
    const a = renderEditorLink(ctx, { text: "x", path: "/w/x" });
    click(a);
    await settle();
    expect(a.hasAttribute("data-arm")).toBe(false);
  });

  it("draws the refusal at the link on an error arm", async () => {
    const { ctx } = harness("error");
    const a = renderEditorLink(ctx, { text: "x", path: "/w/x" });
    click(a);
    await settle();
    expect(refusalAt(a)?.getAttribute("data-arm")).toBe("pathEscapesWorkspace");
  });

  it.each(oneofArms(OpenInEditorErrorSchema, "cause"))(
    "labels the %s arm and says something about it",
    async (arm) => {
      const { ctx } = harness("error", "invalidUrl", arm);
      const a = renderEditorLink(ctx, { text: "x", path: "/w/x" });
      click(a);
      await settle();
      const refusal = refusalAt(a);
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent === ""]).toEqual([
        arm,
        false,
      ]);
    },
  );

  it("says a path outside the workspace is outside it", async () => {
    const { ctx } = harness("error", "invalidUrl", "pathEscapesWorkspace");
    const a = renderEditorLink(ctx, { text: "x", path: "/elsewhere" });
    click(a);
    await settle();
    expect(refusalAt(a)?.textContent).toContain("outside this workspace");
  });

  it("warns on a refusal", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => {}, (level, line) => lines.push([level, line])));
    const { ctx } = harness("error");
    click(renderEditorLink(ctx, { text: "x", path: "/w/x" }));
    await settle();
    expect(lines.some(([level, line]) => level === "warn" && line.includes("link.open-in-editor-refused"))).toBe(true);
  });

  it("says so when the daemon could not be reached", async () => {
    const { ctx } = harness("throw");
    const a = renderEditorLink(ctx, { text: "x", path: "/w/x" });
    click(a);
    await settle();
    expect(refusalAt(a)?.getAttribute("data-arm")).toBe("transport");
  });

  it("leaves a modified click to the platform", async () => {
    const { ctx, editor } = harness();
    const a = renderEditorLink(ctx, { text: "x", path: "/w/x" });
    a.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, metaKey: true }));
    await settle();
    expect(editor).toHaveLength(0);
  });
});

describe("openExternalRefusal: an arm this build has no words for", () => {
  it("refuses an arm a newer daemon added rather than drawing a generic sentence", () => {
    // ARRANGE / ACT / ASSERT
    expect(() =>
      openExternalRefusal({ case: "somethingNewer", value: {} } as never),
    ).toThrow(MalformedView);
  });

  it("names OpenExternalError.cause as the path of the arm it could not draw", () => {
    try {
      openExternalRefusal({ case: "somethingNewer", value: {} } as never);
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("OpenExternalError.cause");
    }
  });
});

describe("openInEditorRefusal: an arm this build has no words for", () => {
  it("refuses an arm a newer daemon added rather than drawing a generic sentence", () => {
    expect(() =>
      openInEditorRefusal({ case: "somethingNewer", value: {} } as never),
    ).toThrow(MalformedView);
  });

  it("names OpenInEditorError.cause as the path of the arm it could not draw", () => {
    try {
      openInEditorRefusal({ case: "somethingNewer", value: {} } as never);
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("OpenInEditorError.cause");
    }
  });
});
