/**
 * The task board, end to end through a real shim.
 *
 * WHAT THIS FILE EXISTS FOR. `AgentTaskAct.state` is "where the act LEFT the
 * task, resolved by the producer" — one resolved snapshot per act, so no
 * consumer replays a sequence to learn what a task currently is. A tracker
 * call produces TWO frames for one act (the announcement and the terminal,
 * both upserting the same unit), and they can be re-delivered: the shim's
 * stream is not the only path a consumer sees them on.
 *
 * So an ANNOUNCEMENT that claims the status the call ASKED FOR is a claim that
 * can win over the tracker's own answer. It did: a `TaskUpdate` the tracker
 * REFUSED drew a ticked checklist row in the running application, because the
 * optimistic announcement arrived again after the refusal. Caught by the G52
 * playbook; asserted here on the frames the shim actually emits.
 */
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  freshSession,
  openStream,
  startTurnRequest,
  watchAgentRequest,
} from "../integration-support/client.js";
import { entryFrame, watchAgentEntry } from "../integration-support/expect.js";

afterEach(cleanupShims);

/** Every task act the turn emitted, in the order the stream carried them. */
function taskActs(
  frames: Iterable<shimv1.WatchAgentResponse>,
): conversationv1.AgentTaskAct[] {
  const acts: conversationv1.AgentTaskAct[] = [];
  for (const frame of frames) {
    if (frame.frame.case !== "entry") continue;
    const agentFrame = entryFrame(frame.frame.value);
    if (agentFrame?.result.case !== "update") continue;
    const update = agentFrame.result.value.update;
    if (update.case !== "activity") continue;
    const item = update.value.item;
    if (item.case !== "taskAct") continue;
    acts.push(item.value);
  }
  return acts;
}

/** Run one prompt to its terminal and answer the task acts it produced. */
async function actsFor(text: string): Promise<conversationv1.AgentTaskAct[]> {
  const shim = await spawnShim();
  await shim.clients.h1.startSession(freshSession());
  const watch = openStream((options) => shim.clients.h1.watchAgent(watchAgentRequest(), options));
  await watch.next();
  await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text }));
  await watch.until((f) => {
    if (f.frame.case !== "entry") return false;
    const inner = watchAgentEntry(f).entry?.entry;
    if (inner?.case !== "agentFrame") return false;
    return inner.value.result.case === "success" || inner.value.result.case === "failure";
  });
  const acts = taskActs(watch.frames());
  watch.close();
  return acts;
}

describe("a task update the tracker refuses", () => {
  test("never states the status the refused act asked for", async () => {
    // Arrange / Act: `!task-reject` is a `TaskUpdate(9, completed)` the board
    // answers `success:false`.
    const acts = await actsFor("!task-reject");

    // Assert: not one frame of the pair claims `completed` — not the
    // announcement, which has left the task nowhere, and not the terminal,
    // which carries the task as it STILL STANDS.
    expect(acts.length).toBeGreaterThan(0);
    expect(acts.map((act) => act.state?.status.case)).not.toContain("completed");
  });

  test("settles as the rejected arm", async () => {
    // Arrange / Act
    const acts = await actsFor("!task-reject");

    // Assert: the act's own outcome is the refusal, whatever the announcement
    // said about it.
    expect(acts.at(-1)?.act.case).toBe("rejected");
  });
});

describe("a task update the tracker takes", () => {
  test("states the status only once the tracker has answered it", async () => {
    // Arrange / Act: `!task-change` moves task 1 to `in_progress`.
    const acts = await actsFor("!task-change");

    // Assert: the announcement leaves the status unset and the terminal
    // carries the tracker's own `statusChange`, so a re-delivery of either can
    // only ever agree with the board.
    expect(acts.map((act) => act.state?.status.case)).toEqual([undefined, "running"]);
  });
});
