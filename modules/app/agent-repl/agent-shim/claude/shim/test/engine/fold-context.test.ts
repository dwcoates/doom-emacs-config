/**
 * What the engine tells the fold.
 *
 * WHAT THIS GUARDS: that the scaffold fold still ENDS turns. A fold wired in
 * that never reported a turn's end would leave every session permanently
 * mid-turn — a worse lie than producing no records, because the daemon would
 * refuse every subsequent prompt as "one is already in flight".
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import { turnBoundaryOnlyFold, type FoldContext } from "../../src/engine/fold-context.js";
import { initMessage, resultMessage } from "./fakes.js";

const CONTEXT: FoldContext = {
  mainAgentId: create(conversationv1.AgentIdSchema, { value: "agent-1" }),
  keepalive: false,
  nowMs: () => 1,
  pendingAsk: () => undefined,
    deniedCall: () => false,
  liveTask: () => undefined,
};

describe("the scaffold fold", () => {
  it("converts nothing — producing frames is the record plane's brief", () => {
    expect(turnBoundaryOnlyFold().onSdkMessage(initMessage(), CONTEXT).entries).toEqual([]);
  });

  it("does not end a turn on an ordinary message", () => {
    expect(turnBoundaryOnlyFold().onSdkMessage(initMessage(), CONTEXT).turnEnded).toBeUndefined();
  });

  it("ENDS the turn on the vendor's result", () => {
    expect(turnBoundaryOnlyFold().onSdkMessage(resultMessage(), CONTEXT).turnEnded).toBeDefined();
  });

  it("attributes the terminal to the main agent", () => {
    const output = turnBoundaryOnlyFold().onSdkMessage(resultMessage(), CONTEXT);

    expect(output.turnEnded?.frame.agentId?.value).toBe("agent-1");
  });

  it("states no terminal arm — the taxonomy is the record plane's to map", () => {
    const output = turnBoundaryOnlyFold().onSdkMessage(resultMessage(), CONTEXT);

    expect(output.turnEnded?.frame.result.case).toBeUndefined();
  });
});

describe("the scaffold fold at a query's end", () => {
  it("has nothing to let go", () => {
    expect(turnBoundaryOnlyFold().endQuery("the vendor query died")).toBeUndefined();
  });
});
