// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { codeOf } from "../../source-text.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
  type SelectWorkspaceResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import {
  OpenWorkspaceResponseSchema,
  type OpenWorkspaceResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { jumpToWorkspace, openAndJumpToWorkspace, type JumpSite } from "../../../src/feed/merge/jump.js";
import { testAppContext } from "../../rpc/app-context.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import { RecordingSink, WORKSPACE, mergeRow, rowContext } from "../harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** A site that is not the queue's, so the parameters are seen to be used. */
const SITE: JumpSite = { describe: "a test switch", operation: "merge.test-jump", refusalClass: "merge-test-refusal" };

/** The workspace every jump selects. */
const TARGET = create(WorkspaceRefSchema, { id: "ws-merge", dir: "/w/merge" });

/** A context whose SelectWorkspace answers ANSWER, or throws THROWN. */
function scripted(answer: SelectWorkspaceResponse | Error) {
  const calls: SelectWorkspaceRequest[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      selectWorkspace: (req) => {
        calls.push(req);
        if (answer instanceof Error) throw answer;
        return answer;
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: createTicker(1000),
    failures: new RecordingSink(),
    composerEnabled: false,
  });
  return { rc: rowContext(ctx, mergeRow("m1")), calls };
}

/** Run one jump from a fresh host to its end. */
async function jump(answer: SelectWorkspaceResponse | Error) {
  const s = scripted(answer);
  const host = document.createElement("div");
  const call = jumpToWorkspace(host, s.rc, TARGET, SITE);
  await vi.advanceTimersByTimeAsync(0);
  await call;
  return { host, calls: s.calls };
}

const SELECTED = create(SelectWorkspaceResponseSchema, { result: { case: "success", value: {} } });

describe("jumpToWorkspace", () => {
  it("selects the workspace verbatim and draws nothing on success", async () => {
    const { host, calls } = await jump(SELECTED);
    expect(calls.map((c) => c.workspace?.id)).toEqual(["ws-merge"]);
    expect(host.children).toHaveLength(0);
  });

  it("draws a refusal in the site's class at the host", async () => {
    const { host } = await jump(
      create(SelectWorkspaceResponseSchema, { result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } } }),
    );
    expect(host.querySelector(".merge-test-refusal")?.getAttribute("data-arm")).toBe("unknownWorkspace");
  });

  it("records a refusal under the site's operation", async () => {
    const capture = captureLogRecords();
    await jump(
      create(SelectWorkspaceResponseSchema, { result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } } }),
    );
    const record = await forwardedRecord(capture, "merge.test-jump-refused");
    expect(record.level.case).toBe("warn");
  });

  it("records an expected answer under the site's operation and draws nothing", async () => {
    const capture = captureLogRecords();
    const { host } = await jump(
      create(SelectWorkspaceResponseSchema, { result: { case: "error", value: { cause: { case: "standingDown", value: {} } } } }),
    );
    expect(host.children).toHaveLength(0);
    const record = await forwardedRecord(capture, "merge.test-jump-expected-answer");
    expect(record.level.case).toBe("info");
  });

  it("draws the transport refusal when the daemon cannot be reached", async () => {
    const { host } = await jump(new ConnectError("socket is gone", Code.Unavailable));
    expect(host.querySelector(".merge-test-refusal")?.textContent).toBe("the daemon could not be reached");
  });

  it("replaces a previous refusal at the host rather than stacking them", async () => {
    const s = scripted(new ConnectError("socket is gone", Code.Unavailable));
    const host = document.createElement("div");
    for (let i = 0; i < 2; i += 1) {
      const call = jumpToWorkspace(host, s.rc, TARGET, SITE);
      await vi.advanceTimersByTimeAsync(0);
      await call;
    }
    expect(host.querySelectorAll(".merge-test-refusal")).toHaveLength(1);
  });

  it("records a transport failure at ERROR under the site's operation", async () => {
    const capture = captureLogRecords();
    await jump(new ConnectError("socket is gone", Code.Unavailable));
    const record = await forwardedRecord(capture, "merge.test-jump-failed");
    expect(record.level.case).toBe("error");
  });

  it("is the one SelectWorkspace call site in the merge bubble", () => {
    for (const file of ["queue.ts", "merge.ts", "merge-body.ts", "workspace-switch.ts", "step-tabs.ts"]) {
      const source = codeOf(readFileSync(join(process.cwd(), "src/feed/merge", file), "utf8"));
      expect(source.includes("selectWorkspace("), file).toBe(false);
    }
  });
});

/** A context whose OpenWorkspace answers OPENED and whose SelectWorkspace answers SELECTED. */
function scriptedOpen(opened: OpenWorkspaceResponse | Error) {
  const calls: string[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openWorkspace: (req) => {
        calls.push(`open ${req.workspace?.id}`);
        if (opened instanceof Error) throw opened;
        return opened;
      },
      selectWorkspace: (req) => {
        calls.push(`select ${req.workspace?.id}`);
        return SELECTED;
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: createTicker(1000),
    failures: new RecordingSink(),
    composerEnabled: false,
  });
  return { rc: rowContext(ctx, mergeRow("m1")), calls };
}

/** Run one open-and-jump from a fresh host to its end. */
async function openAndJump(opened: OpenWorkspaceResponse | Error) {
  const s = scriptedOpen(opened);
  const host = document.createElement("div");
  const call = openAndJumpToWorkspace(host, s.rc, TARGET, SITE);
  await vi.advanceTimersByTimeAsync(0);
  await call;
  return { host, calls: s.calls };
}

const OPENED = create(OpenWorkspaceResponseSchema, { result: { case: "success", value: {} } });
const OPEN_REFUSED = create(OpenWorkspaceResponseSchema, {
  result: { case: "error", value: { cause: { case: "spawnFailed", value: { detail: "no shim" } } } },
});

describe("openAndJumpToWorkspace", () => {
  it("opens the workspace, then selects it, and draws nothing on success", async () => {
    const { host, calls } = await openAndJump(OPENED);
    expect([calls, host.children.length]).toEqual([["open ws-merge", "select ws-merge"], 0]);
  });

  it("selects nothing when the open is refused, and draws the open's own sentence", async () => {
    const { host, calls } = await openAndJump(OPEN_REFUSED);
    const refusal = host.querySelector(".merge-test-refusal");
    expect([calls, refusal?.getAttribute("data-arm"), refusal?.textContent]).toEqual([
      ["open ws-merge"],
      "spawnFailed",
      "the session could not be started: no shim",
    ]);
  });

  it("records a refused open under the site's operation, naming the rpc", async () => {
    const capture = captureLogRecords();
    await openAndJump(OPEN_REFUSED);
    const record = await forwardedRecord(capture, "merge.test-jump-refused");
    expect(record.level.case).toBe("warn");
    expect(JSON.stringify(record)).toContain("OpenWorkspace");
  });

  it("draws a cross-cutting refusal in the shared words", async () => {
    const { host } = await openAndJump(
      create(OpenWorkspaceResponseSchema, { result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } } }),
    );
    expect(host.querySelector(".merge-test-refusal")?.getAttribute("data-arm")).toBe("unknownWorkspace");
  });

  it("selects nothing and records the failure when the daemon cannot be reached", async () => {
    const capture = captureLogRecords();
    const { host, calls } = await openAndJump(new ConnectError("socket is gone", Code.Unavailable));
    expect([calls, host.querySelector(".merge-test-refusal")?.textContent]).toEqual([
      ["open ws-merge"],
      "the daemon could not be reached",
    ]);
    const record = await forwardedRecord(capture, "merge.test-jump-failed");
    expect(record.level.case).toBe("error");
  });

  it("replaces a previous refusal at the host rather than stacking them", async () => {
    const s = scriptedOpen(OPEN_REFUSED);
    const host = document.createElement("div");
    for (let i = 0; i < 2; i += 1) {
      const call = openAndJumpToWorkspace(host, s.rc, TARGET, SITE);
      await vi.advanceTimersByTimeAsync(0);
      await call;
    }
    expect(host.querySelectorAll(".merge-test-refusal")).toHaveLength(1);
  });
});
