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
  type OpenInEditorWorkspaceFile,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import {
  FeedMergeTestLogSchema,
  type FeedMergeTestLog,
} from "../../proto/gen/ts/frontend/v1/feed_pb";
import { oneofArms } from "./arms.js";
import { WorkspaceRefSchema } from "../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../src/clock.js";
import type { FailureSink } from "../src/failure/sink.js";
import { ForwardingLogger, setLogger } from "../src/log.js";
import {
  installProseLinkRouting,
  openExternalRefusal,
  openInEditorRefusal,
  renderEditorLink,
  renderExternalLink,
  renderMergeTestLogLink,
} from "../src/link.js";
import { renderMarkdown } from "../src/markdown.js";
import { createAgentReplClient } from "../src/rpc/client.js";
import { MalformedView } from "../src/rpc/malformed.js";
import { type AppContext } from "../src/rpc/context.js";
import { testAppContext } from "./rpc/app-context.js";

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
  const ctx = testAppContext({
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

/** The workspace-file target a request carried; any other arm fails the test. */
function workspaceFileOf(req: OpenInEditorRequest): OpenInEditorWorkspaceFile {
  if (req.target.case !== "workspaceFile") {
    throw new Error(`expected a workspaceFile target, got ${String(req.target.case)}`);
  }
  return req.target.value;
}

/** A served test-log link, token and label as the merge bubble carries them. */
function testLog(token = "log-7f3a", label = "~/.claude-emacs/merge-logs/ws-tests-2.log"): FeedMergeTestLog {
  return create(FeedMergeTestLogSchema, { token: { value: token }, label: { text: label } });
}

describe("renderMergeTestLogLink", () => {
  it("renders an anchor", () => {
    const { ctx } = harness();
    expect(renderMergeTestLogLink(ctx, testLog(), "log").tagName).toBe("A");
  });

  it("draws the daemon's label as its text", () => {
    const { ctx } = harness();
    const a = renderMergeTestLogLink(ctx, testLog("t", "~/logs/x.log"), "log");
    expect(a.textContent).toBe("~/logs/x.log");
  });

  it("carries the merge test log hook", () => {
    const { ctx } = harness();
    expect(renderMergeTestLogLink(ctx, testLog(), "log").hasAttribute("data-merge-test-log")).toBe(true);
  });

  it("wears the link class the stylesheet paints in the response bubble's link blue", () => {
    const { ctx } = harness();
    expect(renderMergeTestLogLink(ctx, testLog(), "log").classList.contains("merge-test-log-link")).toBe(true);
  });

  it("carries NO href, since the log is on the daemon's host", () => {
    const { ctx } = harness();
    expect(renderMergeTestLogLink(ctx, testLog(), "log").hasAttribute("href")).toBe(false);
  });

  it("does not show the token anywhere in the markup", () => {
    const { ctx } = harness();
    const a = renderMergeTestLogLink(ctx, testLog("secret-token", "~/x.log"), "log");
    expect(a.outerHTML.includes("secret-token")).toBe(false);
  });

  it("is reachable by keyboard", () => {
    const { ctx } = harness();
    expect(renderMergeTestLogLink(ctx, testLog(), "log").tabIndex).toBe(0);
  });

  it("cancels the click", () => {
    const { ctx } = harness();
    expect(click(renderMergeTestLogLink(ctx, testLog(), "log")).defaultPrevented).toBe(true);
  });

  it("sends the merge_test_log target", async () => {
    const { ctx, editor } = harness();
    click(renderMergeTestLogLink(ctx, testLog(), "log"));
    await settle();
    expect(editor[0].target.case).toBe("mergeTestLog");
  });

  it("echoes the token exactly as served", async () => {
    const { ctx, editor } = harness();
    click(renderMergeTestLogLink(ctx, testLog("opaque/ token=1"), "log"));
    await settle();
    const target = editor[0].target;
    expect(target.case === "mergeTestLog" ? target.value.value : null).toBe("opaque/ token=1");
  });

  it("addresses the request to this page's workspace", async () => {
    const { ctx, editor } = harness();
    click(renderMergeTestLogLink(ctx, testLog(), "log"));
    await settle();
    expect(editor[0].workspace?.id).toBe("ws-1");
  });

  it("draws an unknown log's refusal at the link", async () => {
    const { ctx } = harness("error", "invalidUrl", "unknownMergeTestLog");
    const a = renderMergeTestLogLink(ctx, testLog(), "log");
    click(a);
    await settle();
    expect(refusalAt(a)?.textContent).toContain("no longer available");
  });

  it("warns on a refusal, naming the target kind", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { ctx } = harness("error", "invalidUrl", "unknownMergeTestLog");
    click(renderMergeTestLogLink(ctx, testLog(), "log"));
    await settle();
    expect(
      lines.some(
        ([level, line]) =>
          level === "warn" && line.includes("link.open-in-editor-refused") && line.includes("mergeTestLog"),
      ),
    ).toBe(true);
  });

  it("errors when the daemon could not be reached", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { ctx } = harness("throw");
    click(renderMergeTestLogLink(ctx, testLog(), "log"));
    await settle();
    expect(lines.some(([level, line]) => level === "error" && line.includes("link.open-in-editor-failed"))).toBe(true);
  });

  it("refuses a log with no token", () => {
    const { ctx } = harness();
    const u = create(FeedMergeTestLogSchema, { label: { text: "~/x.log" } });
    expect(() => renderMergeTestLogLink(ctx, u, "log")).toThrow(MalformedView);
  });

  it("refuses a log with no label", () => {
    const { ctx } = harness();
    const u = create(FeedMergeTestLogSchema, { token: { value: "t" } });
    expect(() => renderMergeTestLogLink(ctx, u, "log")).toThrow(MalformedView);
  });

  it("leaves a modified click to the platform", async () => {
    const { ctx, editor } = harness();
    const a = renderMergeTestLogLink(ctx, testLog(), "log");
    a.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, metaKey: true }));
    await settle();
    expect(editor).toHaveLength(0);
  });

  it("is left alone by the prose interceptor, opening once", async () => {
    const { ctx, editor } = harness();
    const root = document.createElement("div");
    root.appendChild(renderMergeTestLogLink(ctx, testLog(), "log"));
    const off = installProseLinkRouting(ctx, root);
    click(root.querySelector("a")!);
    await settle();
    expect(editor).toHaveLength(1);
    off();
  });
});

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
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
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
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
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

  it("sends the workspace_file target", async () => {
    const { ctx, editor } = harness();
    click(renderEditorLink(ctx, { text: "x", path: "/w/x" }));
    await settle();
    expect(editor[0].target.case).toBe("workspaceFile");
  });

  it("relays the path verbatim inside the workspace_file target", async () => {
    const { ctx, editor } = harness();
    click(renderEditorLink(ctx, { text: "x", path: "/w/a b/c.md" }));
    await settle();
    expect(workspaceFileOf(editor[0]).path).toBe("/w/a b/c.md");
  });

  it("sends the target's line when one was given", async () => {
    const { ctx, editor } = harness();
    click(renderEditorLink(ctx, { text: "x", path: "/w/x", line: 42 }));
    await settle();
    expect(workspaceFileOf(editor[0]).line).toBe(42);
  });

  it("leaves the target's line UNSET when none was given, rather than sending a zero", async () => {
    // ARRANGE: unset means the file's top; a zero would claim line zero exists.
    const { ctx, editor } = harness();
    // ACT
    click(renderEditorLink(ctx, { text: "x", path: "/w/x" }));
    await settle();
    // ASSERT
    expect(workspaceFileOf(editor[0]).line).toBeUndefined();
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

  // `linkUnresolved` is the one arm drawn NOWHERE on this page: the daemon
  // publishes the footer line and asks the question in the conversation.
  it.each(oneofArms(OpenInEditorErrorSchema, "cause").filter((arm) => arm !== "linkUnresolved"))(
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
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
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

describe("installProseLinkRouting: a clicked markdown prose link", () => {
  /**
   * A feed-scroll stand-in with a bubble of markdown-rendered HTML inside it,
   * the interceptor installed over the whole thing exactly as the boot hangs
   * it on `feedScroll`.
   */
  function prose(ctx: AppContext, html: string): { root: HTMLElement; off: () => void } {
    const root = document.createElement("div");
    const bubble = document.createElement("div");
    bubble.innerHTML = html;
    root.appendChild(bubble);
    const off = installProseLinkRouting(ctx, root);
    return { root, off };
  }

  it("cancels the click, so the webview never navigates", () => {
    // Arrange.
    const { ctx } = harness();
    const { root, off } = prose(ctx, renderMarkdown("[docs](https://example.test/x)"));
    const anchor = root.querySelector("a")!;

    // Act.
    const event = click(anchor);

    // Assert.
    expect(event.defaultPrevented).toBe(true);
    off();
  });

  it("routes an http(s) link through OpenExternal", async () => {
    // Arrange.
    const { ctx, external } = harness();
    const { root, off } = prose(ctx, renderMarkdown("[docs](https://example.test/x)"));

    // Act.
    click(root.querySelector("a")!);
    await settle();

    // Assert.
    expect(external).toHaveLength(1);
    expect(external[0].url).toBe("https://example.test/x");
    off();
  });

  it("routes a file:// link through OpenInEditor, decoding its path", async () => {
    // Arrange: markdown restricts anchors to http(s), so a file link is
    // injected as the raw anchor the interceptor must still route.
    const { ctx, editor } = harness();
    const { root, off } = prose(ctx, `<a href="file:///Users/u/w/my%20file.go">my file.go</a>`);

    // Act.
    click(root.querySelector("a")!);
    await settle();

    // Assert.
    expect(editor).toHaveLength(1);
    expect(workspaceFileOf(editor[0]).path).toBe("/Users/u/w/my file.go");
    off();
  });

  /** A prose anchor drawn inside a feed row, as a bubble draws it. */
  function proseInRow(ctx: AppContext, rowId: string, html: string): { root: HTMLElement; off: () => void } {
    const root = document.createElement("div");
    const row = document.createElement("div");
    row.setAttribute("data-feed-row", rowId);
    row.innerHTML = html;
    root.appendChild(row);
    return { root, off: installProseLinkRouting(ctx, root) };
  }

  function feedLinkOf(req: OpenInEditorRequest): { href: string; row: string } {
    if (req.target.case !== "feedLink") throw new Error(`expected a feedLink target, got ${String(req.target.case)}`);
    return { href: req.target.value.href, row: req.target.value.sourceRow?.value ?? "" };
  }

  it("routes a bare absolute-path link as a feed_link carrying the source row", async () => {
    const { ctx, editor } = harness();
    const { root, off } = proseInRow(ctx, "row-7", `<a href="/Users/u/w/a.go">a.go</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(editor.map(feedLinkOf)).toEqual([{ href: "/Users/u/w/a.go", row: "row-7" }]);
    off();
  });

  it("routes a bare file name as a feed_link, href verbatim", async () => {
    const { ctx, editor } = harness();
    const { root, off } = proseInRow(ctx, "row-7", `<a href="AGENTS.md">AGENTS.md</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(editor.map(feedLinkOf)).toEqual([{ href: "AGENTS.md", row: "row-7" }]);
    off();
  });

  it("sends a `:<line>` suffix inside the href untouched", async () => {
    const { ctx, editor } = harness();
    const { root, off } = proseInRow(ctx, "row-7", `<a href="lisp/status.el:42">status</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(editor.map(feedLinkOf)).toEqual([{ href: "lisp/status.el:42", row: "row-7" }]);
    off();
  });

  it("names the innermost row when feed rows nest", async () => {
    const { ctx, editor } = harness();
    const { root, off } = proseInRow(
      ctx,
      "outer",
      `<div data-feed-row="inner"><a href="a.go">a</a></div>`,
    );
    click(root.querySelector("a")!);
    await settle();
    expect(editor.map(feedLinkOf)[0]?.row).toBe("inner");
    off();
  });

  it("draws nothing beside the link when the daemon answers link_unresolved", async () => {
    const { ctx } = harness("error", "invalidUrl", "linkUnresolved");
    const { root, off } = proseInRow(ctx, "row-7", `<a href="nope.go">nope</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(root.querySelector(".refusal")).toBeNull();
    off();
  });

  it("logs the unresolved answer rather than swallowing it", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { ctx } = harness("error", "invalidUrl", "linkUnresolved");
    const { root, off } = proseInRow(ctx, "row-7", `<a href="nope.go">nope</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(lines.some(([level, line]) => level === "info" && line.includes("link.feed-link-unresolved"))).toBe(true);
    off();
  });

  it("opens no rpc and warns for a path link that sits in no feed row", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { ctx, editor } = harness();
    const { root, off } = prose(ctx, `<a href="a.go">a</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect({ calls: editor.length, warned: lines.some(([l, m]) => l === "warn" && m.includes("link.feed-link-no-row")) }).toEqual({ calls: 0, warned: true });
    off();
  });

  it("sends report for an anchor with no web-fallback mark", async () => {
    const { ctx, editor } = harness();
    const { root, off } = proseInRow(ctx, "row-7", `<a href="a.go">a</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(editor[0]?.target.case === "feedLink" ? editor[0].target.value.onUnresolved.case : null).toBe("report");
    off();
  });

  it("sends web_fallback for an anchor marked ambiguous", async () => {
    const { ctx, editor } = harness();
    const { root, off } = proseInRow(ctx, "row-7", `<a href="wikipedia.org" data-web-fallback>w</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(editor[0]?.target.case === "feedLink" ? editor[0].target.value.onUnresolved.case : null).toBe("webFallback");
    off();
  });

  it("opens http://<name> through OpenExternal when a web_fallback link is unresolved", async () => {
    const { ctx, external } = harness("error", "invalidUrl", "linkUnresolved");
    const { root, off } = proseInRow(ctx, "row-7", `<a href="wikipedia.org" data-web-fallback>w</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(external.map((r) => r.url)).toEqual(["http://wikipedia.org"]);
    off();
  });

  it("opens nothing on the web when a web_fallback link RESOLVES", async () => {
    const { ctx, external } = harness();
    const { root, off } = proseInRow(ctx, "row-7", `<a href="README.md" data-web-fallback>r</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(external).toHaveLength(0);
    off();
  });

  it("opens nothing on the web when a report link is unresolved", async () => {
    const { ctx, external } = harness("error", "invalidUrl", "linkUnresolved");
    const { root, off } = proseInRow(ctx, "row-7", `<a href="nope.go">n</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect(external).toHaveLength(0);
    off();
  });

  it("keeps a web link on OpenExternal inside a feed row", async () => {
    const { ctx, external, editor } = harness();
    const { root, off } = proseInRow(ctx, "row-7", `<a href="https://example.test/x">x</a>`);
    click(root.querySelector("a")!);
    await settle();
    expect({ external: external.length, editor: editor.length }).toEqual({ external: 1, editor: 0 });
    off();
  });

  it("renders a markdown path link as an anchor the router can take", () => {
    expect(renderMarkdown("[a](lisp/status.el)")).toContain('<a href="lisp/status.el"');
  });

  it("leaves a structured external link to its own handler, opening it once", async () => {
    // Arrange: a structured link inside the same scroll zone must not be
    // double-fired by the delegated interceptor.
    const { ctx, external } = harness();
    const root = document.createElement("div");
    root.appendChild(renderExternalLink(ctx, { text: "docs", url: "https://example.test/s" }));
    const off = installProseLinkRouting(ctx, root);

    // Act.
    click(root.querySelector("a")!);
    await settle();

    // Assert.
    expect(external).toHaveLength(1);
    off();
  });

  it("leaves a structured editor link to its own handler, opening it once", async () => {
    // Arrange.
    const { ctx, editor } = harness();
    const root = document.createElement("div");
    root.appendChild(renderEditorLink(ctx, { text: "a.go", path: "/w/a.go" }));
    const off = installProseLinkRouting(ctx, root);

    // Act.
    click(root.querySelector("a")!);
    await settle();

    // Assert.
    expect(editor).toHaveLength(1);
    off();
  });

  it("warns and ignores a prose link whose scheme is neither web nor file", () => {
    // Arrange.
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { ctx, external, editor } = harness();
    const { root, off } = prose(ctx, `<a href="mailto:a@b.test">mail</a>`);

    // Act.
    click(root.querySelector("a")!);

    // Assert.
    expect(external).toHaveLength(0);
    expect(editor).toHaveLength(0);
    expect(
      lines.some(([level, line]) => level === "warn" && line.includes("link.prose-unroutable-scheme")),
    ).toBe(true);
    off();
  });
});
