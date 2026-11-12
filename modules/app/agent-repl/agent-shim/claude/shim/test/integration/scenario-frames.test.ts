/**
 * test/integration/scenario-frames.test.ts — what the shim emits for the
 * fake-SDK scenarios the e2e suite used to drive through the whole stack.
 *
 * Each case drives the real shim process with one `--fake` scenario and asserts
 * the frames it puts on its own surfaces: the WatchAgent stream, the
 * WatchSession stream and the store's inbox. What the DAEMON draws from those
 * frames is the daemon integration suite's to assert (`daemon/integration`),
 * with the fake shim speaking these same frames.
 */
import { create } from "@bufbuild/protobuf";
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  allowStanding,
  freshSession,
  openSessionUpdates,
  openStream,
  startTurnRequest,
  watchAgentRequest,
} from "../integration-support/client.js";
import {
  entryFrame,
  sessionStarted,
  sessionUpdate,
  watchAgentEntry,
} from "../integration-support/expect.js";
import { pageBookOf, pageLineOf, writtenEntries } from "../integration-support/store.js";
import { readTranscript } from "../integration-support/vendor.js";

afterEach(cleanupShims);

type Shim = Awaited<ReturnType<typeof spawnShim>>;
type AgentStream = ReturnType<typeof openStream<shimv1.WatchAgentResponse>>;

/** Open the main agent's stream past its opening page. */
async function openAgentStream(shim: Shim): Promise<AgentStream> {
  const stream = openStream((options) => shim.clients.h1.watchAgent(watchAgentRequest(), options));
  await stream.next();
  return stream;
}

function isTerminal(frame: shimv1.WatchAgentResponse): boolean {
  if (frame.frame.case !== "entry") return false;
  const agentFrame = entryFrame(watchAgentEntry(frame));
  return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
}

/** Every AgentFrame the stream has carried so far, in order. */
function framesOf(stream: AgentStream): conversationv1.AgentFrame[] {
  return stream
    .frames()
    .flatMap((frame) => (frame.frame.case === "entry" ? [entryFrame(watchAgentEntry(frame))] : []))
    .filter((frame): frame is conversationv1.AgentFrame => frame !== null);
}

/** A fresh session, one turn driven to its terminal, and every frame that turn put on the stream. */
async function driveTurn(
  text: string,
): Promise<{ shim: Shim; frames: conversationv1.AgentFrame[]; vendorSessionId: string }> {
  const shim = await spawnShim();
  const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
  const stream = await openAgentStream(shim);
  await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text }));
  await stream.until(isTerminal);
  const frames = framesOf(stream);
  stream.close();
  return { shim, frames, vendorSessionId: started.vendorSessionId };
}

/** Every activity the frames carry, in order. */
function activitiesOf(frames: readonly conversationv1.AgentFrame[]): conversationv1.AgentActivity[] {
  return frames.flatMap((frame) =>
    frame.result.case === "update" && frame.result.value.update.case === "activity"
      ? [frame.result.value.update.value]
      : [],
  );
}

/** The prose of every settled response in the activities. */
function responseProse(activities: readonly conversationv1.AgentActivity[]): string[] {
  return activities.flatMap((activity) =>
    activity.item.case === "response" && activity.item.value.result.case === "success"
      ? [activity.item.value.result.value.prose?.markdown ?? ""]
      : [],
  );
}

function terminalArm(frames: readonly conversationv1.AgentFrame[]): string | undefined {
  const terminal = frames.find((frame) => frame.result.case === "success" || frame.result.case === "failure");
  if (terminal?.result.case === "success") return `success.${terminal.result.value.outcome.case}`;
  if (terminal?.result.case === "failure") return `failure.${terminal.result.value.cause.case}`;
  return undefined;
}

describe("a withheld-unnamed slash-command record", () => {
  test("puts nothing of the record on the stream: the turn's prose and its terminal are all it carries", async () => {
    const { frames } = await driveTurn("!slash-shape-a-unnamed");

    const items = [
      ...new Set(
        activitiesOf(frames)
          .map((activity) => activity.item.case)
          .filter((arm) => arm !== "thinking"),
      ),
    ];
    expect({
      items,
      prose: responseProse(activitiesOf(frames)),
      terminal: terminalArm(frames),
    }).toEqual({
      items: ["response"],
      prose: ["Recorded the withheld-unnamed bookkeeping."],
      terminal: "success.completed",
    });
  });

  test("lands in the vendor's own transcript as a user record naming no command", async () => {
    const { shim, vendorSessionId } = await driveTurn("!slash-shape-a-unnamed");

    const record = readTranscript(shim.dirs, vendorSessionId).find(
      (candidate) =>
        candidate.type === "user" &&
        (candidate.message as { content?: unknown } | undefined)?.content ===
          "<local-command-stdout>total 4\ndrwxr-xr-x</local-command-stdout>",
    );

    expect(record).toBeDefined();
    expect(JSON.stringify(record)).not.toContain("<command-name>");
  });
});

/** Every activity of the `item` arm, in the order its frames arrived. */
function activitiesNamed(
  activities: readonly conversationv1.AgentActivity[],
  item: string,
): conversationv1.AgentActivity[] {
  return activities.filter((activity) => activity.item.case === item);
}

describe("an MCP server's tool call", () => {
  test("is an AgentMcpToolCall that starts and settles with the tool's name and its returned text", async () => {
    const { frames } = await driveTurn("!mcp-tool");

    const calls = activitiesNamed(activitiesOf(frames), "mcpToolCall").map((activity) =>
      activity.item.case === "mcpToolCall" ? activity.item.value.result : undefined,
    );
    const settled = calls.find((result) => result?.case === "success");
    expect({
      arms: calls.map((result) => result?.case),
      tool: settled?.case === "success" ? settled.value.tool?.name : undefined,
      text:
        settled?.case === "success"
          ? settled.value.content?.blocks.map((block) =>
              block.block.case === "text" ? block.block.value.text : undefined,
            )
          : undefined,
    }).toEqual({
      arms: ["start", "success"],
      tool: "mcp__echo__echo",
      text: ["hello from the offline session"],
    });
  });

  test("is never an unmodeled activity", async () => {
    const { frames } = await driveTurn("!mcp-tool");

    expect(activitiesNamed(activitiesOf(frames), "unmodeled")).toEqual([]);
  });
});
