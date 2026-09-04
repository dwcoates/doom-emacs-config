/**
 * §F3 — SUB-FEED PLUMBING, against the real chain.
 *
 * The feed is the one SELF-SIMILAR component: a subagent bubble IS a feed,
 * with its own pages and its own live tail, and the bubble row's `FeedId` is
 * simultaneously the sub-feed's address. What only the webapp can cover is
 * the LIFECYCLE of that address — expand issues `OpenFeed` on the bubble's
 * own id and draws its rows inside the bubble's own container; collapse
 * abandons the token and the rows leave.
 *
 * The bubble here is a real one: the `subagent` scenario a Go area test
 * already drives, run by the real shim, resolved by the real daemon.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import {
  BOOT_BUDGET_MS,
  TURN_TEST_MS,
  awaitDrawn,
  bootLayer,
  driveTurn,
  rows,
  textOf,
} from "./drive";

let app: MountedApp;

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

/**
 * Drive one real subagent turn and answer its bubble row.
 *
 * Each test drives its OWN bubble rather than sharing one, because expanding
 * and collapsing is exactly what these tests mutate: a shared bubble would
 * make one test's final state another test's arrangement.
 */
async function subagentBubble(): Promise<HTMLElement> {
  return driveTurn(app, "subagent", "activity", "subagent");
}

/** The bubble's own FeedId, which is also its sub-feed's address. */
function bubbleId(row: HTMLElement): string {
  const id = row.dataset.feedRow;
  if (id === undefined || id === "") throw new Error("the subagent row carries no FeedId");
  return id;
}

// §F3 #16.
it(
  "draws the sub-feed's own rows inside the bubble's own container when expanded",
  async () => {
    // Arrange
    const row = await subagentBubble();
    const id = bubbleId(row);
    expect(app.feedContainer(id), "a collapsed bubble holds no sub-feed").toBeNull();

    // Act — the app's own expand control, which issues OpenFeed on this id.
    await app.click(`[data-feed-row="${id}"] [data-expand]`);

    // Assert — the sub-feed exists at the bubble's address, and the rows the
    // daemon answered are drawn INSIDE it (a sub-feed's rows arrive on the
    // bubble's own feed and never name the bubble).
    await awaitDrawn(app, `the sub-feed at ${id}`, () => app.feedContainer(id) !== null);
    const container = app.feedContainer(id);
    expect(container).not.toBeNull();
    await awaitDrawn(
      app,
      `rows inside the sub-feed at ${id}`,
      () => app.rowIds(container ?? undefined).length > 0,
    );
    expect(app.rowIds(container ?? undefined).length).toBeGreaterThan(0);
  },
  TURN_TEST_MS,
);

// §F3 #17. COLLAPSE ABANDONS THE TOKEN AND KEEPS THE DOM
// (src/feed/bubble.ts `collapse`), so what the page can be held to is the
// fold: the row and the bubble both say collapsed and the sub-feed's panel is
// hidden. That the WATCH was cancelled is the daemon's own surface and the Go
// suite's to assert — the page has no way to observe a cancelled stream.
it(
  "folds the sub-feed away when the bubble is collapsed, keeping its address",
  async () => {
    // Arrange — a bubble expanded against the real daemon.
    const row = await subagentBubble();
    const id = bubbleId(row);
    await app.click(`[data-feed-row="${id}"] [data-expand]`);
    await awaitDrawn(app, `the sub-feed at ${id}`, () => app.feedContainer(id) !== null);

    // Act
    await app.click(`[data-feed-row="${id}"] [data-expand]`);

    // Assert — the row states the fold, and the sub-feed is no longer shown.
    await awaitDrawn(
      app,
      `the bubble at ${id} to say it is collapsed`,
      () => app.$(`[data-feed-row="${id}"]`)?.dataset.expanded === "false",
    );
    const container = app.feedContainer(id);
    expect(container, "collapse keeps the sub-feed's DOM at its own address").not.toBeNull();
    expect(container?.closest("[hidden]") ?? container?.hasAttribute("hidden")).toBeTruthy();
  },
  TURN_TEST_MS,
);

// §F3 #18.
it(
  "re-opens the same sub-feed address after a collapse, drawing its rows again",
  async () => {
    // Arrange — expanded, then collapsed.
    const row = await subagentBubble();
    const id = bubbleId(row);
    await app.click(`[data-feed-row="${id}"] [data-expand]`);
    await awaitDrawn(app, `the sub-feed at ${id}`, () => app.feedContainer(id) !== null);
    const first = app.rowIds(app.feedContainer(id) ?? undefined);
    await app.click(`[data-feed-row="${id}"] [data-expand]`);
    await awaitDrawn(
      app,
      `the bubble at ${id} to say it is collapsed`,
      () => app.$(`[data-feed-row="${id}"]`)?.dataset.expanded === "false",
    );

    // Act — a settled bubble opens again from the daemon's own walk position;
    // the client asks for a page, never for a cursor.
    await app.click(`[data-feed-row="${id}"] [data-expand]`);

    // Assert — the same address answers the same rows.
    await awaitDrawn(
      app,
      `the sub-feed at ${id} to be re-opened`,
      () => app.$(`[data-feed-row="${id}"]`)?.dataset.expanded === "true",
    );
    expect(app.rowIds(app.feedContainer(id) ?? undefined)).toEqual(first);
  },
  TURN_TEST_MS,
);

// §F3, the parity invariant stated as a test: the ROOT feed is addressed
// "root" and a bubble by its FeedId, and both are the same component.
it(
  "keeps the root feed's own rows out of the bubble's container",
  async () => {
    // Arrange
    const row = await subagentBubble();
    const id = bubbleId(row);

    // Act
    await app.click(`[data-feed-row="${id}"] [data-expand]`);
    await awaitDrawn(app, `the sub-feed at ${id}`, () => app.feedContainer(id) !== null);

    // Assert — the bubble's container holds only the sub-feed's rows, and the
    // root's own turn rows are not among them.
    const inside = app.rowIds(app.feedContainer(id) ?? undefined);
    const rootTurnEnded = rows(app, "turnEnded").map((el) => el.dataset.feedRow ?? "");
    for (const terminal of rootTurnEnded) expect(inside).not.toContain(terminal);
  },
  TURN_TEST_MS,
);

/**
 * The commission the fake gives every spawned agent, sync and detached alike
 * (`agent-shim/claude/shim/src/fake/scenarios/subagents.ts`), read verbatim so
 * the assertion pins the INSTRUCTION and not a paraphrase of it.
 */
const COMMISSION = "Do the sweep and report.";

/**
 * Expand a spawn bubble and answer the `agent_prompt` row drawn inside its own
 * container.
 *
 * THE COMMISSION IS A SUB-FEED ROW, never a root one: a bubble's body IS its
 * sub-feed, so the instruction is addressed to the CREATED agent and drawn on
 * that agent's feed with a "from <sender>" address line.
 */
async function commissionRow(row: HTMLElement): Promise<HTMLElement> {
  const id = bubbleId(row);
  await app.click(`[data-feed-row="${id}"] [data-expand]`);
  await awaitDrawn(app, `the sub-feed at ${id}`, () => app.feedContainer(id) !== null);
  const container = app.feedContainer(id);
  expect(container).not.toBeNull();
  await awaitDrawn(
    app,
    `an agentPrompt row inside the sub-feed at ${id}`,
    () => (container?.querySelectorAll('[data-row-kind="agentPrompt"]').length ?? 0) > 0,
  );
  const drawn = container?.querySelectorAll<HTMLElement>('[data-row-kind="agentPrompt"]') ?? [];
  const last = drawn[drawn.length - 1];
  expect(last, "the sub-feed drew no agentPrompt row").toBeDefined();
  return last as HTMLElement;
}

// §F3 #19 — the SYNC spawn's commission, drawn in the bubble's body.
it(
  "draws a sync spawn's commission as an agent_prompt row inside its bubble",
  async () => {
    // Arrange / Act
    const commission = await commissionRow(await subagentBubble());

    // Assert — the instruction verbatim, under an address naming the sender.
    expect(textOf(commission)).toContain(COMMISSION);
    expect(textOf(commission.querySelector(".prompt-address"))).toContain("from ");
  },
  TURN_TEST_MS,
);

// §F3 #20 — the DETACHED spawn's commission. Sync-versus-detached is placement,
// so the body draws the same way through the same sub-feed address.
it(
  "draws a detached spawn's commission as an agent_prompt row inside its bubble",
  async () => {
    // Arrange / Act
    const row = await driveTurn(app, "subagent-detached", "detachedSubagent");
    const commission = await commissionRow(row);

    // Assert
    expect(textOf(commission)).toContain(COMMISSION);
    expect(textOf(commission.querySelector(".prompt-address"))).toContain("from ");
  },
  TURN_TEST_MS,
);
