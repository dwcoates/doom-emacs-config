// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  FindPullRequestOwnerResponseSchema,
  type FindPullRequestOwnerResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_find_pull_request_owner_pb";
import { OpenExternalResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import { SelectInEditorResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_in_editor_pb";
import { OpenWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  installPullRequestOwnerClicks,
  jumpToPullRequestOwner,
  type PullRequestOwnerClick,
} from "../../src/feed/pull-request-owner.js";
import { scriptedAppContext } from "../rpc/app-context.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { RecordingSink, WORKSPACE } from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

const URL = "https://github.com/ChessCom/explanation-engine/pull/42";
const OWNER = create(WorkspaceRefSchema, { id: "ws-owner", dir: "/w/owner" });

function owned(standing: "open" | "closed"): FindPullRequestOwnerResponse {
  return create(FindPullRequestOwnerResponseSchema, {
    result: { case: "success", value: { answer: { case: "owned", value: { workspace: OWNER, standing: { case: standing, value: {} } } } } },
  });
}

function answered(answer: "unowned" | "notPullRequest"): FindPullRequestOwnerResponse {
  return create(FindPullRequestOwnerResponseSchema, {
    result: { case: "success", value: { answer: { case: answer, value: {} } } },
  });
}

/** Shift-click once: the calls the page made, in order, and the link. */
async function shiftClick(found: FindPullRequestOwnerResponse | Error) {
  const calls: string[] = [];
  const ctx = scriptedAppContext(
    {
      findPullRequestOwner: (req) => {
        calls.push(`find ${req.url}`);
        if (found instanceof Error) throw found;
        return found;
      },
      openWorkspace: (req) => {
        calls.push(`open ${req.workspace?.id}`);
        return create(OpenWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
      selectInEditor: (req) => {
        calls.push(`select ${req.workspace?.id} from ${req.followedFrom?.id}`);
        return create(SelectInEditorResponseSchema, { result: { case: "success", value: { route: { case: "relayed", value: {} } } } });
      },
    },
    { workspace: WORKSPACE, failures: new RecordingSink() },
  );
  const anchor = document.createElement("a");
  const call = jumpToPullRequestOwner(ctx, anchor, URL);
  let outcome: PullRequestOwnerClick | Error;
  call.then((o) => (outcome = o), (e: Error) => (outcome = e));
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
  return { calls, anchor, outcome: outcome! };
}

describe("jumpToPullRequestOwner", () => {
  it("selects an open owner, as a link followed from this page, and draws nothing", async () => {
    const { calls, anchor, outcome } = await shiftClick(owned("open"));
    expect([calls, anchor.children.length, outcome]).toEqual([[`find ${URL}`, `select ws-owner from ${WORKSPACE.id}`], 0, "handled"]);
  });

  it("reopens a closed owner, then selects it", async () => {
    const { calls, outcome } = await shiftClick(owned("closed"));
    expect([calls, outcome]).toEqual([[`find ${URL}`, "open ws-owner", `select ws-owner from ${WORKSPACE.id}`], "handled"]);
  });

  it("says at the link that nobody owns the pull request, and switches to nothing", async () => {
    const { calls, anchor, outcome } = await shiftClick(answered("unowned"));
    const refusal = anchor.querySelector(".pull-request-owner-refusal");
    expect([calls, refusal?.getAttribute("data-arm"), refusal?.textContent, outcome]).toEqual([
      [`find ${URL}`],
      "unowned",
      "no workspace owns this pull request",
      "handled",
    ]);
  });

  it("records an unowned pull request with its url", async () => {
    const capture = captureLogRecords();
    await shiftClick(answered("unowned"));
    const record = await forwardedRecord(capture, "feed.pull-request-owner-unowned");
    expect([record.level.case, record.context?.url]).toEqual(["info", URL]);
  });

  it("answers a url naming no pull request back to the caller, drawing nothing", async () => {
    const { calls, anchor, outcome } = await shiftClick(answered("notPullRequest"));
    expect([calls, anchor.children.length, outcome]).toEqual([[`find ${URL}`], 0, "notPullRequest"]);
  });

  it("draws the transport refusal at the link, recorded at ERROR naming the rpc, when the daemon cannot be reached", async () => {
    const capture = captureLogRecords();
    const { anchor, outcome } = await shiftClick(new ConnectError("socket is gone", Code.Unavailable));
    const record = await forwardedRecord(capture, "rpc.unary-transport-failure");
    expect([
      anchor.querySelector(".pull-request-owner-refusal")?.getAttribute("data-arm"),
      record.level.case,
      record.context?.rpc,
      outcome,
    ]).toEqual(["transport", "error", "FindPullRequestOwner", "handled"]);
  });

  it("refuses an owner with no standing as a malformed view", async () => {
    const found = owned("open");
    if (found.result.case !== "success" || found.result.value.answer.case !== "owned") throw new Error("arrange");
    found.result.value.answer.value.standing = { case: undefined };
    const { calls, outcome } = await shiftClick(found);
    expect([calls, outcome instanceof MalformedView]).toEqual([[`find ${URL}`], true]);
  });

  it("refuses an answer with no arm as a malformed view", async () => {
    const { outcome } = await shiftClick(create(FindPullRequestOwnerResponseSchema, { result: { case: "success", value: {} } }));
    expect(outcome instanceof MalformedView).toBe(true);
  });

  it("replaces a previous refusal at the link rather than stacking them", async () => {
    const { anchor } = await shiftClick(answered("unowned"));
    const ctx = scriptedAppContext(
      { findPullRequestOwner: () => answered("unowned") },
      { workspace: WORKSPACE, failures: new RecordingSink() },
    );
    const call = jumpToPullRequestOwner(ctx, anchor, URL);
    for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
    await call;
    expect(anchor.querySelectorAll(".pull-request-owner-refusal")).toHaveLength(1);
  });
});

describe("installPullRequestOwnerClicks", () => {
  /** A feed scroll zone holding one prose link to HREF, with the interceptor and a stand-in prose router on it. */
  function scope(href: string, answer: FindPullRequestOwnerResponse) {
    const calls: string[] = [];
    const ctx = scriptedAppContext(
      {
        findPullRequestOwner: (req) => {
          calls.push(`find ${req.url}`);
          return answer;
        },
        openExternal: (req) => {
          calls.push(`external ${req.url}`);
          return create(OpenExternalResponseSchema, { result: { case: "success", value: {} } });
        },
        selectInEditor: (req) => {
          calls.push(`select ${req.workspace?.id}`);
          return create(SelectInEditorResponseSchema, { result: { case: "success", value: { route: { case: "relayed", value: {} } } } });
        },
      },
      { workspace: WORKSPACE, failures: new RecordingSink() },
    );
    const root = document.createElement("div");
    root.innerHTML = `<p><a href="${href}">the link</a></p>`;
    document.body.append(root);
    // The prose router hangs on the scope itself, installed FIRST here, so the
    // interceptor is seen to run ahead of it whatever the order.
    const routed: string[] = [];
    const router = (event: MouseEvent): void => {
      if (!event.defaultPrevented) routed.push("prose router");
    };
    root.addEventListener("click", router, true);
    // jsdom has no navigation to make for a click nothing claimed.
    root.addEventListener("click", (event) => event.preventDefault());
    const off = installPullRequestOwnerClicks(ctx, root);
    const teardown = () => {
      off();
      root.removeEventListener("click", router, true);
      root.remove();
    };
    return { root, calls, routed, teardown };
  }

  async function click(root: HTMLElement, init: MouseEventInit): Promise<MouseEvent> {
    const event = new MouseEvent("click", { bubbles: true, cancelable: true, button: 0, ...init });
    root.querySelector("a")!.dispatchEvent(event);
    for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
    return event;
  }

  it("takes a shift-click ahead of the prose router, cancels it, and switches to the owner", async () => {
    const s = scope(URL, owned("open"));
    const event = await click(s.root, { shiftKey: true });
    expect([event.defaultPrevented, s.routed, s.calls]).toEqual([true, [], [`find ${URL}`, "select ws-owner"]]);
    s.teardown();
  });

  it("opens a shift-clicked link naming no pull request as a plain click does", async () => {
    const s = scope("https://example.test/x", answered("notPullRequest"));
    await click(s.root, { shiftKey: true });
    expect(s.calls).toEqual(["find https://example.test/x", "external https://example.test/x"]);
    s.teardown();
  });

  it("leaves a plain click to the prose router", async () => {
    const s = scope(URL, owned("open"));
    await click(s.root, {});
    expect([s.routed, s.calls]).toEqual([["prose router"], []]);
    s.teardown();
  });

  it("leaves a shift-click with another modifier to the prose router", async () => {
    const s = scope(URL, owned("open"));
    await click(s.root, { shiftKey: true, metaKey: true });
    expect([s.routed, s.calls]).toEqual([["prose router"], []]);
    s.teardown();
  });

  it("leaves a shift-click on a link outside its scope alone", async () => {
    const s = scope(URL, owned("open"));
    const outside = document.createElement("a");
    outside.href = URL;
    document.body.append(outside);
    const event = new MouseEvent("click", { bubbles: true, cancelable: true, button: 0, shiftKey: true });
    outside.addEventListener("click", (e) => e.preventDefault());
    outside.dispatchEvent(event);
    for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
    expect(s.calls).toEqual([]);
    outside.remove();
    s.teardown();
  });

  it("takes nothing once uninstalled", async () => {
    const s = scope(URL, owned("open"));
    s.teardown();
    document.body.append(s.root);
    await click(s.root, { shiftKey: true });
    expect(s.calls).toEqual([]);
    s.root.remove();
  });
});
