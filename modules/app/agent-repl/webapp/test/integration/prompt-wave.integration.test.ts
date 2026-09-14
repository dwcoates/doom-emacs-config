/**
 * THE PROMPT BUBBLE'S THINKING WAVE ACROSS ONE TURN, on the booted app.
 *
 * The wave is a WORKING prompt's wave (breathing.ts), and the defect it is
 * checked against here was the stylesheet running it on `.bubble.user`
 * outright: every prompt in the scrollback animated forever, so a settled
 * conversation had no resting state at all.
 *
 * WHY AN INTEGRATION CASE ON TOP OF THE UNIT ONES. The mark is set by the feed
 * controller and consumed by a selector in the shipped stylesheet, and the two
 * are only one mechanism if the attribute the controller writes is the
 * attribute the sheet reads. This boots the whole app, drives a real turn
 * through the fake daemon's tail, and asserts BOTH ends: the attribute on the
 * bubble, and the `bubble-wave` rule keyed on exactly it.
 */
import { afterEach, describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { PROMPT_WAVE_ATTRIBUTE, PROMPT_WAVE_WORKING } from "../../src/breathing";
import { startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import {
  WORKSPACE_ID,
  feedId,
  responseRow,
  turnEndedConcludedRow,
  turnId,
  userPromptRow,
} from "./fixtures";

let harness: Harness;

afterEach(async () => {
  await harness?.stop();
});

/** The prompt bubble drawn for row ID. */
function bubble(id: string): HTMLElement {
  const row = harness.row(id);
  if (row === null) throw new Error(`the feed drew no row for ${id}`);
  const el = row.querySelector<HTMLElement>(".bubble.user");
  if (el === null) throw new Error(`row ${id} drew no prompt bubble`);
  return el;
}

describe("the prompt bubble's thinking wave, across a turn", () => {
  it("waves the prompt while its turn runs and stops when the turn concludes", async () => {
    // Arrange — the app booted on a live tail, the prompt delivered.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      userPromptRow("do the thing", { id: feedId("p1"), turn: turnId("t1") }),
    );
    await harness.settle();
    const working = bubble("p1").getAttribute(PROMPT_WAVE_ATTRIBUTE);
    const drawn = bubble("p1");

    // Act — the turn ends, on the same tail the prompt arrived on.
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      turnEndedConcludedRow(feedId("p1"), { id: feedId("e1"), turn: turnId("t1") }),
    );
    await harness.settle();

    // Assert — the band ran, then stopped, and the bubble it stopped on is the
    // very element that was already on screen: the prompt was never redrawn.
    expect({
      whileRunning: working,
      afterEnding: bubble("p1").getAttribute(PROMPT_WAVE_ATTRIBUTE),
      sameElement: bubble("p1") === drawn,
      text: bubble("p1").textContent?.includes("do the thing"),
    }).toEqual({
      whileRunning: PROMPT_WAVE_WORKING,
      afterEnding: null,
      sameElement: true,
      text: true,
    });
  });

  it("holds the wave through the turn's own final answer, releasing it only at turn_ended", async () => {
    // THE BOUNDARY THE 'settled too early' REPORT NAMES: a turn's answering
    // response landing must NOT settle the prompt — the feed marks the final
    // answer only when the `turn_ended` row is drawn (turn-ended.ts), so a
    // response arriving `success` while the turn is still open is the agent's
    // answer taking shape, not the turn ending. The one prior case ends the
    // turn on the push AFTER the prompt; this drives the realistic order —
    // prompt, a streaming update, the settled answer, THEN the turn's end — and
    // asserts the band survives every step until the last.
    //
    // Arrange — the app booted on a live tail, the prompt delivered.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      userPromptRow("do the thing", { id: feedId("p1"), turn: turnId("t1") }),
    );
    await harness.settle();
    const afterPrompt = bubble("p1").getAttribute(PROMPT_WAVE_ATTRIBUTE);

    // Act 1 — the answer streams in, then settles, both on the prompt's turn.
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      responseRow("update", "thinking", { id: feedId("r1"), turn: turnId("t1") }),
    );
    await harness.settle();
    const afterStreaming = bubble("p1").getAttribute(PROMPT_WAVE_ATTRIBUTE);
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      responseRow("success", "the answer", { id: feedId("r1"), turn: turnId("t1") }),
    );
    await harness.settle();
    const afterFinalResponse = bubble("p1").getAttribute(PROMPT_WAVE_ATTRIBUTE);

    // Act 2 — the turn ends, naming that settled response as its answer.
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      turnEndedConcludedRow(feedId("r1"), { id: feedId("e1"), turn: turnId("t1") }),
    );
    await harness.settle();

    // Assert — waving from draw, through the streamed and the settled answer,
    // and only the turn's end takes it away.
    expect({
      afterPrompt,
      afterStreaming,
      afterFinalResponse,
      afterTurnEnded: bubble("p1").getAttribute(PROMPT_WAVE_ATTRIBUTE),
    }).toEqual({
      afterPrompt: PROMPT_WAVE_WORKING,
      afterStreaming: PROMPT_WAVE_WORKING,
      afterFinalResponse: PROMPT_WAVE_WORKING,
      afterTurnEnded: null,
    });
  });

  it("keys the shipped bubble-wave rule on the mark and on nothing wider", () => {
    // Arrange — the stylesheet as it ships, not a copy. Comments come out
    // first: they are where this sheet keeps its reasoning, and a selector
    // read with one still attached is the comment, not the selector.
    const css = readFileSync(resolve(process.cwd(), "src/styles.css"), "utf8").replace(
      /\/\*[\s\S]*?\*\//g,
      "",
    );

    // Act — every rule that runs the wave animation.
    const selectors = [...css.matchAll(/([^{}]*)\{[^{}]*animation:\s*bubble-wave[^{}]*\}/g)].map(
      (match) => match[1].trim(),
    );

    // Assert — there is exactly one, and it demands the mark.
    expect(selectors).toEqual([
      `.bubble.user[${PROMPT_WAVE_ATTRIBUTE}="${PROMPT_WAVE_WORKING}"]`,
    ]);
  });
});
