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
  if (terminal?.result.case === "failure") return `failure.${terminal.result.value.failure.case}`;
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

describe("the vendor's diagnostics report after a Write", () => {
  test("arrives as AgentWrite.diagnostics on the write call's own activity, after its settled success", async () => {
    const { frames } = await driveTurn("!ide-diagnostics-write");

    const writes = activitiesNamed(activitiesOf(frames), "write").map((activity) => ({
      id: activity.activityId?.value,
      result: activity.item.case === "write" ? activity.item.value.result : undefined,
    }));
    const diagnostics = writes.find((write) => write.result?.case === "diagnostics");
    expect({
      arms: writes.map((write) => write.result?.case),
      sameActivity: new Set(writes.map((write) => write.id)).size,
      messages:
        diagnostics?.result?.case === "diagnostics"
          ? diagnostics.result.value.files.flatMap((file) =>
              file.diagnostics.map((diagnostic) => [
                file.path,
                diagnostic.severity,
                diagnostic.message,
                diagnostic.source,
              ]),
            )
          : undefined,
    }).toEqual({
      arms: ["start", "success", "diagnostics"],
      sameActivity: 1,
      messages: [
        [
          "/w/s/created.ts",
          conversationv1.AgentDiagnosticSeverity.ERROR,
          "Cannot find name 'missing'.",
          "typescript",
        ],
      ],
    });
  });
});

/** The settled arms of every AgentHook activity, with the activity id each rode on. */
function hookArms(
  frames: readonly conversationv1.AgentFrame[],
): Array<{ id: string | undefined; hook: conversationv1.AgentHook }> {
  return activitiesOf(frames).flatMap((activity) =>
    activity.item.case === "hook" ? [{ id: activity.activityId?.value, hook: activity.item.value }] : [],
  );
}

describe("a blocking PostToolUse hook around an Edit", () => {
  test("announces its firing and then states the refusal on the same activity", async () => {
    const { frames } = await driveTurn("!hook-blocked");

    const hooks = hookArms(frames);
    const [start, outcome] = hooks;
    expect({
      arms: hooks.map((entry) => entry.hook.result.case),
      oneActivity: new Set(hooks.map((entry) => entry.id)).size,
      name: start?.hook.result.case === "start" ? start.hook.result.value.hookName : undefined,
      event: start?.hook.result.case === "start" ? start.hook.result.value.event : undefined,
      blockingText:
        outcome?.hook.result.case === "blockingError" ? outcome.hook.result.value.blockingText : undefined,
    }).toEqual({
      arms: ["start", "blockingError"],
      oneActivity: 1,
      name: "PostToolUse:Edit",
      event: conversationv1.AgentHookEvent.POST_TOOL_USE,
      blockingText: "the suite failed after the edit",
    });
  });

  test("settles the gated Edit as a failure while the turn still concludes", async () => {
    const { frames } = await driveTurn("!hook-blocked");

    const edit = activitiesOf(frames)
      .filter((activity) => activity.item.case === "edit")
      .map((activity) => (activity.item.case === "edit" ? activity.item.value.result.case : undefined));
    expect({ edit, terminal: terminalArm(frames) }).toEqual({
      edit: ["start", "failure"],
      terminal: "success.completed",
    });
  });
});

describe("a failing SessionStart hook", () => {
  test("states a non-blocking error carrying its exit code and its stderr", async () => {
    const { frames } = await driveTurn("!hook-failed");

    const hooks = hookArms(frames);
    const outcome = hooks.find((entry) => entry.hook.result.case === "nonBlockingError")?.hook.result;
    expect({
      arms: hooks.map((entry) => entry.hook.result.case),
      exitCode: outcome?.case === "nonBlockingError" ? outcome.value.exitCode : undefined,
      stderr: outcome?.case === "nonBlockingError" ? outcome.value.output?.stderr : undefined,
    }).toEqual({
      arms: ["start", "nonBlockingError"],
      exitCode: 1,
      stderr: "Failed to run: no interpreter on PATH.",
    });
  });

  test("blocks nothing: the turn concludes", async () => {
    const { frames } = await driveTurn("!hook-failed");

    expect(terminalArm(frames)).toBe("success.completed");
  });
});

/** Every permission arm the frames carry, with the decision a settled one states. */
function permissionArms(frames: readonly conversationv1.AgentFrame[]): string[] {
  return frames.flatMap((frame) => {
    if (frame.result.case !== "update" || frame.result.value.update.case !== "permission") return [];
    const permission = frame.result.value.update.value;
    if (permission.result.case === "success") {
      const decision = permission.result.value.decision;
      return [decision.case === "denied" ? `denied.${decision.value.by.case}` : `success.${decision.case}`];
    }
    return [permission.result.case ?? "unset"];
  });
}

describe("a query that dies with a permission ask open", () => {
  test("opens the ask, then settles it denied by the user before the turn ends", async () => {
    const { frames } = await driveTurn("!query-eof-mid-ask");

    expect(permissionArms(frames)).toEqual(["start", "denied.user"]);
  });

  test("ends the turn on the query_died failure arm carrying unexpected_eof", async () => {
    const { frames } = await driveTurn("!query-eof-mid-ask");

    const terminal = frames.find((frame) => frame.result.case === "failure");
    const failure = terminal?.result.case === "failure" ? terminal.result.value.failure : undefined;
    expect({
      arm: failure?.case,
      cause: failure?.case === "queryDied" ? failure.value.cause.case : undefined,
    }).toEqual({ arm: "queryDied", cause: "unexpectedEof" });
  });

  test("reports the death on the session stream as query_died.unexpected_eof", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!query-eof-mid-ask" }));
    const died = await watch.until((frame) => sessionUpdate(frame).update.case === "queryDied");

    const update = sessionUpdate(died).update;
    expect(update.case === "queryDied" ? update.value.cause.case : undefined).toBe("unexpectedEof");
    watch.close();
  });
});

describe("a converter fault and its recovery", () => {
  /** The degraded windows a diagnostics push carries. */
  function windowsOf(update: conversationv1.SessionUpdate): conversationv1.SessionDegradedWindow[] {
    return update.update.case === "diagnostics" ? update.update.value.degradedWindows : [];
  }

  test("the window an unconvertible hook opens is the window the next clean turn closes, with the dropped count", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!fault-converter" }));
    const opened = windowsOf(
      sessionUpdate(
        await watch.until((frame) =>
          windowsOf(sessionUpdate(frame)).some((window) => window.extent.case === "open"),
        ),
      ),
    ).find((window) => window.extent.case === "open");

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!fault-recover" }));
    const closed = windowsOf(
      sessionUpdate(
        await watch.until((frame) =>
          windowsOf(sessionUpdate(frame)).some(
            (window) => window.extent.case === "closed" && window.beganAtMs === opened?.beganAtMs,
          ),
        ),
      ),
    ).find((window) => window.extent.case === "closed" && window.beganAtMs === opened?.beganAtMs);

    expect(opened).toBeDefined();
    expect({
      component: closed?.component,
      droppedAtLeastOne:
        closed?.extent.case === "closed" ? closed.extent.value.droppedCount >= 1n : undefined,
      endedNotBeforeItBegan:
        closed?.extent.case === "closed" ? closed.extent.value.endedAtMs >= (opened?.beganAtMs ?? 0n) : undefined,
    }).toEqual({ component: opened?.component, droppedAtLeastOne: true, endedNotBeforeItBegan: true });
    watch.close();
  });

  test("the unconvertible hook produces no frame while the turn still concludes", async () => {
    const { frames } = await driveTurn("!fault-converter");

    expect({ hooks: hookArms(frames), terminal: terminalArm(frames) }).toEqual({
      hooks: [],
      terminal: "success.completed",
    });
  });

  test("the same hook, well-formed, is a succeeded firing the shim does not put on the stream", async () => {
    const { frames } = await driveTurn("!fault-recover");

    expect({ hooks: hookArms(frames), terminal: terminalArm(frames) }).toEqual({
      hooks: [],
      terminal: "success.completed",
    });
  });
});

describe("an allow-standing answer to a gated Bash", () => {
  test("lets the gated Bash run to a completed success and the turn conclude", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-allow-standing" }));
    const ask = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      return (
        agentFrame?.result.case === "update" &&
        agentFrame.result.value.update.case === "permission" &&
        agentFrame.result.value.update.value.result.case === "start"
      );
    });
    const asked = entryFrame(watchAgentEntry(ask));
    const permission =
      asked?.result.case === "update" && asked.result.value.update.case === "permission"
        ? asked.result.value.update.value
        : undefined;
    const offered = permission?.result.case === "start" ? permission.result.value.offeredStanding : undefined;
    if (permission === undefined || offered === undefined) throw new Error("the ask offered no standing");

    await shim.clients.h1.updateAgent(allowStanding(create(conversationv1.AgentPermissionIdSchema, { value: permission.id?.value ?? "" }), offered));
    await stream.until(isTerminal);

    const frames = framesOf(stream);
    const bash = activitiesOf(frames)
      .filter((activity) => activity.item.case === "bash")
      .map((activity) => {
        const result = activity.item.case === "bash" ? activity.item.value.result : undefined;
        return result?.case === "success" ? `success.${result.value.outcome.case}` : result?.case;
      });
    expect({ permission: permissionArms(frames), bash, terminal: terminalArm(frames) }).toEqual({
      permission: ["start", "success.allowed"],
      bash: ["start", "success.completed"],
      terminal: "success.completed",
    });
    stream.close();
  });
});

/** The AgentFrames the shim wrote to the store, each with the agent book its page line files it under. */
function storedFrames(shim: Shim): Array<{ book: string | undefined; frame: conversationv1.AgentFrame }> {
  return writtenEntries(shim.store?.writes() ?? []).flatMap((entry) => {
    const line = pageLineOf(entry);
    return line?.agentItem?.item.case === "agentFrame"
      ? [{ book: pageBookOf(line), frame: line.agentItem.item.value }]
      : [];
  });
}

describe("a synchronous subagent's nested activity", () => {
  const COMMISSION = "Read the module's AGENTS.md and report the test command.";

  test("rides the main agent's book as one subagent unit: its commission at the start, its report at the settle", async () => {
    const { shim, vendorSessionId } = await driveTurn("!subagent");

    const units = storedFrames(shim)
      .filter(({ book }) => book === vendorSessionId)
      .flatMap(({ frame }) => activitiesOf([frame]))
      .filter((activity) => activity.item.case === "subagent");
    const arms = units.map((activity) => (activity.item.case === "subagent" ? activity.item.value.result : undefined));
    const start = arms.find((result) => result?.case === "start");
    const success = arms.find((result) => result?.case === "success");
    expect({
      arms: arms.map((result) => result?.case),
      commission: start?.case === "start" ? start.value.prompt?.text : undefined,
      description: start?.case === "start" ? start.value.prompt?.description : undefined,
      label: start?.case === "start" ? start.value.prompt?.subagentType : undefined,
      report: success?.case === "success" ? success.value.report?.prose?.markdown : undefined,
    }).toEqual({
      arms: ["start", "success"],
      commission: COMMISSION,
      description: "Explore the module",
      label: "general-purpose",
      report: "The module's test command is `npm test`.",
    });
  });

  test("is filed under the subagent's own book: its prose and its Read, none of it on the main book", async () => {
    const { shim, vendorSessionId } = await driveTurn("!subagent");

    const stored = storedFrames(shim);
    const spawner = stored
      .flatMap(({ frame }) => activitiesOf([frame]))
      .find((activity) => activity.item.case === "subagent" && activity.item.value.result.case === "start");
    const spawned =
      spawner?.item.case === "subagent" && spawner.item.value.result.case === "start"
        ? spawner.item.value.result.value.createdAgentId?.value
        : undefined;
    const proseIn = (book: string | undefined): string[] =>
      responseProse(stored.filter((entry) => entry.book === book).flatMap(({ frame }) => activitiesOf([frame])));
    const readsIn = (book: string | undefined): number =>
      stored
        .filter((entry) => entry.book === book)
        .flatMap(({ frame }) => activitiesOf([frame]))
        .filter((activity) => activity.item.case === "read").length;

    expect({
      subBook: { prose: proseIn(spawned), reads: readsIn(spawned) > 0 },
      mainBook: { prose: proseIn(vendorSessionId), reads: readsIn(vendorSessionId) },
    }).toEqual({
      subBook: {
        prose: ["Reading the module's conventions.", "The module's test command is `npm test`."],
        reads: true,
      },
      mainBook: { prose: ["The subagent reported back."], reads: 0 },
    });
  });
});

describe("a foreground Bash", () => {
  test("starts with its command and settles as a completed success carrying its stdout", async () => {
    const { frames } = await driveTurn("!bash");

    const results = activitiesOf(frames)
      .filter((activity) => activity.item.case === "bash")
      .map((activity) => (activity.item.case === "bash" ? activity.item.value.result : undefined));
    const start = results.find((result) => result?.case === "start");
    const success = results.find((result) => result?.case === "success");
    const output = success?.case === "success" ? success.value.outcome : undefined;
    expect({
      arms: results.map((result) => result?.case),
      command: start?.case === "start" ? start.value.command?.line : undefined,
      outcome: output?.case,
      stdout:
        output?.case === "completed" && output.value.output?.form.case === "text"
          ? output.value.output.form.value.stdout
          : undefined,
    }).toEqual({
      arms: ["start", "success"],
      command: "pwd; ls | head",
      outcome: "completed",
      stdout: "one\ntwo\n",
    });
  });

  test("is never announced as detached work", async () => {
    const { frames } = await driveTurn("!bash");

    expect(frames.filter((frame) => frame.result.case === "detachedWork")).toEqual([]);
  });
});
