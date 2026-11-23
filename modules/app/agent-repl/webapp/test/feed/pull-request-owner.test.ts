// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  FindPullRequestOwnerResponseSchema,
  type FindPullRequestOwnerResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_find_pull_request_owner_pb";
import { SelectInEditorResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_in_editor_pb";
import { OpenWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { jumpToPullRequestOwner, type PullRequestOwnerClick } from "../../src/feed/pull-request-owner.js";
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
