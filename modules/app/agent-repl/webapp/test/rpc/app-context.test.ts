// @vitest-environment jsdom
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { codeUnder } from "../files-under.js";
import { RecordingSink, WORKSPACE } from "../feed/harness.js";
import { scriptedAppContext } from "./app-context.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

const REF = create(WorkspaceRefSchema, { id: "ws-a", dir: "/w/a" });
const SELECTED = create(SelectWorkspaceResponseSchema, { result: { case: "success", value: {} } });

/** How many ticks the context's ticker delivers within MS. */
function ticksWithin(tickMs: number | undefined, ms: number): number {
  const ctx = scriptedAppContext({}, { workspace: WORKSPACE, failures: new RecordingSink(), ...(tickMs === undefined ? {} : { tickMs }) });
  let ticks = 0;
  const stop = ctx.ticker.subscribe(() => {
    ticks += 1;
  });
  vi.advanceTimersByTime(ms);
  stop();
  return ticks;
}

describe("scriptedAppContext", () => {
  it("answers a scripted rpc with the script's answer, the request echoed to it", async () => {
    const seen: SelectWorkspaceRequest[] = [];
    const ctx = scriptedAppContext(
      {
        selectWorkspace: (req) => {
          seen.push(req);
          return SELECTED;
        },
      },
      { workspace: WORKSPACE, failures: new RecordingSink() },
    );
    const answer = await ctx.client.selectWorkspace({ workspace: REF });
    expect([answer.result.case, seen.map((r) => r.workspace?.id)]).toEqual(["success", ["ws-a"]]);
  });

  it("refuses an rpc the script does not implement as unimplemented", async () => {
    const ctx = scriptedAppContext({}, { workspace: WORKSPACE, failures: new RecordingSink() });
    const err = await ctx.client.selectWorkspace({ workspace: REF }).catch((e: unknown) => e);
    expect(ConnectError.from(err).code).toBe(Code.Unimplemented);
  });

  it("is the page of the workspace it was given", () => {
    const ctx = scriptedAppContext({}, { workspace: WORKSPACE, failures: new RecordingSink() });
    expect(ctx.workspace).toBe(WORKSPACE);
  });

  it("ticks once a second unless told otherwise", () => {
    expect([ticksWithin(undefined, 999), ticksWithin(undefined, 1000)]).toEqual([0, 1]);
  });

  it("ticks at the period it was told", () => {
    expect([ticksWithin(60_000, 59_999), ticksWithin(60_000, 60_000)]).toEqual([0, 1]);
  });

  it("is how every merge test scripts its daemon, so none builds the client by hand", () => {
    const here = path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "feed", "merge");
    const handRolled = [...codeUnder(here)].filter(([, code]) => code.includes("createRouterTransport")).map(([file]) => file);
    expect(handRolled).toEqual([]);
  });
});
