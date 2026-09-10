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
