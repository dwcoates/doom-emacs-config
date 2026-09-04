/**
 * §F2 — FEED ROW RENDERING, ONE TEST PER DRAWN FAMILY, against the real chain.
 *
 * Every row here was produced by a real turn: the page submitted a real
 * `!scenario` prompt through its own composer, the real shim ran the fake
 * SDK's scenario of that name, the real store and sidecar took the facts, and
 * the real daemon resolved the row this test reads out of the DOM. Nothing is
 * arranged; there is no fixture in this file.
 *
 * SCENARIO NAMES ARE THE GO SUITE'S OWN (project-lead ruling): every `!name`
 * below is one an area test in `e2e/*_e2e_test.go` already drives. The
 * family → scenario mapping was established empirically against this very
 * chain (recorded in `e2e/WEBAPP-LAYER-SPEC.md` §F2), never guessed from a
 * name.
 *
 * ONE FAMILY IS ABSENT AND SAID SO, rather than covered with an invented
 * fixture: `cold_gate`. No Go e2e test references the cold gate at all; it is
 * reported to the fake-SDK owner, and when a scenario lands it becomes one
 * more `it` here. `agent_prompt` IS covered, since landing 10 gave the row a
 * delivery outcome and `!send-message-resumed` drives it.
 *
 * ONE PAGE, ONE WORKSPACE, MANY TURNS. The page is mounted once for the file
 * (the Go driver hands this child exactly one world), so rows accumulate as
 * the file runs and each test reads the NEWEST row of its family. Each test
 * also waits out its own turn, so the next submission is never queued behind
 * a running one.
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
  submit,
  textOf,
} from "./drive";

let app: MountedApp;

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

/** Drive one scenario to a completed turn and read the row it added. */
async function family(scenario: string, kind: string, unit?: string): Promise<HTMLElement> {
  return driveTurn(app, scenario, kind, unit);
}

// §F2 #2 — user_prompt.
it(
  "draws the composer's own submission as a user_prompt row, and no row before the daemon pushes one",
  async () => {
    // Arrange / Act
    const row = await family("md", "userPrompt");
    // Assert — the row carries the text this page sent.
    expect(textOf(row)).toContain("!md");
  },
  TURN_TEST_MS,
);

// §F2 #4 — activity / FeedResponse.
it(
  "draws a settled response row with its body and terminal state",
  async () => {
    // Arrange / Act
    const row = await family("md", "activity", "response");
    // Assert — the markdown showcase body, and the state the row settled in.
    expect(row.querySelector(".bubble-body")).not.toBeNull();
    expect(textOf(row)).toContain("Markdown showcase");
    expect(row.dataset.state).toBe("success");
  },
  TURN_TEST_MS,
);

// NOT ASSERTED HERE, AND DELIBERATELY: the response's USAGE STAMP.
//
// The contract says the usage stamp "rides every state" of a FeedResponse
// (frontend/v1/feed.proto, and docs/overhaul/webapp.md's component map), and
// the renderer draws one whenever `FeedResponse.usage` is set
// (src/feed/cards/response.ts). Against this real chain it is never set: `md`,
// `usage-full` and `prose-streamed` all draw a response with NO `.usage-stamp`
// anywhere on the page. That is a production/contract gap this layer found and
// is reported to the daemon owner — not something an e2e file may paper over
// with a fixture, and not something it may assert red while the fix belongs to
// someone else. When the daemon populates the field, this becomes one more
// `it` asserting `.usage-stamp` on the row above.

// §F2 #5 — activity / FeedSimpleToolCall, one test per output form the
// scenarios produce. The client holds no per-tool knowledge: the same shell
// draws all of them, so what is asserted is the shell's own parts.
it(
  "draws a shell tool call with its composed input line and output body",
  async () => {
    // Arrange / Act
    const row = await family("bash", "activity", "simpleToolCall");
    // Assert — the generic shell's own parts: the composed input line (in its
    // own input FORM) and the output body.
    expect(row.querySelector("[data-input-form]")).not.toBeNull();
    expect(row.querySelector("[data-output-body]")).not.toBeNull();
  },
  TURN_TEST_MS,
);

it(
  "draws an edit tool call's output as diff lines",
  async () => {
    // Arrange / Act
    const row = await family("edit", "activity", "simpleToolCall");
    // Assert — the diff form, painted as typed lines rather than raw text.
    expect(row.querySelectorAll("[data-diff-line]").length).toBeGreaterThan(0);
  },
  TURN_TEST_MS,
);

it(
  "draws a read tool call's output through the same generic shell",
  async () => {
    // Arrange / Act
    const row = await family("read", "activity", "simpleToolCall");
    // Assert
    expect(row.querySelector(".tool-name")).not.toBeNull();
    expect(row.querySelector("[data-output-body]")).not.toBeNull();
  },
  TURN_TEST_MS,
);

it(
  "draws a web-search tool call's results as link rows",
  async () => {
    // Arrange / Act — WEB SEARCH, NOT WEB FETCH. The links form is
    // WebSearch's alone: daemon/internal/resolve/feed/toolcall.go builds a
    // FeedToolCallLinksOutput only from AgentWebSearchSuccess's results
    // (linksForm), while AgentWebFetchSuccess is a fetched PAGE and draws
    // the generic text form. A `!web-fetch` row therefore has no link rows
    // to find, and asserting them there tested nothing this chain produces.
    const row = await family("web-search", "activity", "simpleToolCall");
    // Assert — the LINKS output form, not a text blob. src/feed/cards/
    // tool-call.ts's drawFeedToolCallLinksOutput gives the list
    // `.tool-links` and each result its own `.tool-link-row`, so those
    // selectors are what distinguish this form from the generic text one a
    // `.tool-head` check would have passed for equally.
    expect(row.querySelector(".tool-links")).not.toBeNull();
    expect(row.querySelectorAll(".tool-link-row").length).toBeGreaterThan(0);
    expect(textOf(row)).not.toBe("");
  },
  TURN_TEST_MS,
);

// §F2 #6 — activity / FeedSkill.
it(
  "draws a skill card",
  async () => {
    // Arrange / Act
    const row = await family("skill", "activity", "skill");
    // Assert — the card's own identity, and the row that carries a skill's
    // nested work.
    expect(row.dataset.unit).toBe("skill");
    expect(textOf(row)).not.toBe("");
  },
  TURN_TEST_MS,
);

it(
  "draws a skill's outcome as the card's own state",
  async () => {
    // Arrange / Act
    const row = await family("skill-fail", "activity", "skill");
    // Assert — the card states its outcome arm (src/feed/cards/skill.ts sets
    // `data-state` to the outcome's oneof case), so a cold repaint of the row
    // alone renders the same state.
    // The arm is `failed` SPECIFICALLY: `!skill-fail` names a skill that does
    // not resolve, and any of `running`/`loaded`/`denied` would have satisfied
    // a mere presence check.
    const card = row.querySelector<HTMLElement>("[data-state]");
    expect(card).not.toBeNull();
    expect(card?.dataset.state).toBe("failed");
  },
  TURN_TEST_MS,
);

// §F2 #7 — activity / FeedHook. A SUCCEEDED hook draws NOTHING by contract
// ("quiet automation stays quiet"), so the family is driven by the two
// failing arms the hooks area already drives.
it(
  "draws a blocked hook's card",
  async () => {
    // Arrange / Act
    const row = await family("hook-blocked", "activity", "hook");
    // Assert — the BLOCKED arm specifically. src/feed/cards/hook.ts sets
    // `data-state` to the outcome's oneof case and gives only the blocked arm
    // the loud `.tool-hook-blocked` treatment, and the daemon composes the
    // headline ("hook blocked: <name> (<event>)") into `.tool-head`.
    expect(row.dataset.unit).toBe("hook");
    const card = row.querySelector<HTMLElement>("[data-state]");
    expect(card?.dataset.state).toBe("blocked");
    expect(card?.classList.contains("tool-hook-blocked")).toBe(true);
    expect(textOf(row.querySelector<HTMLElement>(".tool-head"))).not.toBe("");
  },
  TURN_TEST_MS,
);

it(
  "draws a failed hook's card",
  async () => {
    // Arrange / Act
    const row = await family("hook-failed", "activity", "hook");
    // Assert — the FAILED arm specifically, and NOT the blocked arm's loud
    // treatment: the two are different facts and hook.ts draws them
    // differently.
    expect(row.dataset.unit).toBe("hook");
    const card = row.querySelector<HTMLElement>("[data-state]");
    expect(card?.dataset.state).toBe("failed");
    expect(card?.classList.contains("tool-hook-blocked")).toBe(false);
  },
  TURN_TEST_MS,
);

it(
  "draws no hook row for a hook that succeeded",
  async () => {
    // Act — the succeeding hook guards a Read, so the turn's own drawn row is
    // that tool call, and the row carries the turn it belongs to.
    const call = await family("hook-success", "activity", "simpleToolCall");
    const turn = call.dataset.turn;
    expect(turn, "the drawn tool call names no turn").toBeTruthy();

    // Assert — quiet automation stayed quiet: THIS TURN drew no hook row.
    //
    // SCOPED TO THE TURN, NOT COUNTED OVER THE PAGE. One page accumulates
    // every turn this file drives, and a hook row of an EARLIER turn can still
    // be arriving while this one runs — the whole feed is one live stream, and
    // nothing orders another turn's row against this turn's end. A before/after
    // total therefore reads any late neighbour as this scenario's row and fails
    // on a schedule rather than on the contract. The contract itself is
    // per-hook ("a succeeded hook draws NOTHING", daemon bubbles.go drawHook
    // over frontend/v1 FeedTurnActivity.hook), and the turn stamp on every row
    // (feed-view.ts's data-turn) says exactly which hook rows are this
    // scenario's — of which there must be none.
    const mine = rows(app, "activity", "hook").filter((row) => row.dataset.turn === turn);
    expect(mine.map((row) => row.dataset.feedRow ?? "")).toEqual([]);
  },
  TURN_TEST_MS,
);

// §F2 #8 — activity / FeedPlan.
it(
  "draws a plan bubble with its edit links",
  async () => {
    // Arrange / Act
    const row = await family("plan", "activity", "plan");
    // Assert — the plan document, through the ONE shared jump-to-file link
    // component.
    expect(row.querySelector(".plan-prose, .plan-planned")).not.toBeNull();
  },
  TURN_TEST_MS,
);

// §F2 #9 — activity / FeedFindings.
it(
  "draws a findings bubble with one row per finding",
  async () => {
    // Arrange / Act
    const row = await family("findings", "activity", "findings");
    // Assert
    expect(row.querySelectorAll("[data-finding]").length).toBeGreaterThan(0);
  },
  TURN_TEST_MS,
);

// §F2 #10 — activity / FeedArtifact. Only a PUBLISH draws; a list act
// produces no row.
it(
  "draws a published artifact's bubble with its url",
  async () => {
    // Arrange / Act
    const row = await family("artifact-publish", "activity", "artifact");
    // Assert
    expect(row.querySelector(".artifact-url")).not.toBeNull();
  },
  TURN_TEST_MS,
);

it(
  "draws no artifact row for a list act",
  async () => {
    // Arrange
    const before = rows(app, "activity", "artifact").length;
    // Act
    await family("artifact-list", "activity", "response");
    // Assert
    expect(rows(app, "activity", "artifact").length).toBe(before);
  },
  TURN_TEST_MS,
);

// §F2 #11 — activity / FeedSubagent, the sync form.
it(
  "draws a sync subagent's bubble head",
  async () => {
    // Arrange / Act
    const row = await family("subagent", "activity", "subagent");
    // Assert
    expect(row.querySelector(".subagent-head")).not.toBeNull();
  },
  TURN_TEST_MS,
);

// §F2 #12 — the detached wrappers draw the SAME component their sync form
// draws; sync-vs-detached is placement, never a second drawing.
it(
  "draws a detached subagent through the same head the sync form uses",
  async () => {
    // Arrange / Act
    const row = await family("subagent-detached", "detachedSubagent");
    // Assert — the wrapper's own row kind, the sync form's own drawing.
    expect(row.dataset.rowKind).toBe("detachedSubagent");
    expect(row.querySelector(".subagent-head")).not.toBeNull();
  },
  TURN_TEST_MS,
);

it(
  "draws a detached shell through the shell bubble",
  async () => {
    // Arrange / Act
    const row = await family("bash-detach", "detachedShell");
    // Assert
    expect(row.dataset.rowKind).toBe("detachedShell");
    expect(row.querySelector(".shell-bubble, .shell-head")).not.toBeNull();
  },
  TURN_TEST_MS,
);

// §F2 #13 — turn_ended, the terminal fact as a row.
it(
  "draws the turn's terminal row",
  async () => {
    // Arrange
    const before = rows(app, "turnEnded").length;
    // Act
    await family("md", "activity", "response");
    // Assert — exactly one more terminal row than before, and it is a row like
    // any other (history replays it).
    expect(rows(app, "turnEnded").length).toBe(before + 1);
  },
  TURN_TEST_MS,
);

// §F2 #14 — separation, one arm per meta divider. ONE renderer subroutine
// draws every arm, so each arm is asserted through its own class.
it(
  "draws a compaction as a separation row",
  async () => {
    // Arrange / Act
    const row = await family("compact", "separation");
    // Assert
    expect(row.querySelector(".sep-compacted, .sep-label")).not.toBeNull();
  },
  TURN_TEST_MS,
);

it(
  "draws a context rotation as a separation row",
  async () => {
    // Arrange / Act
    const row = await family("rotate", "separation");
    // Assert — the CLEARED arm specifically. `!rotate` is the `/clear`
    // scenario ("SessionIdentityRotated + AgentUpdate.context_cut(
    // ContextCleared)", session.ts), and src/feed/rows/separation.ts stamps
    // the arm onto `data-arm`/`data-state` and picks its rule accent from the
    // same case — so the compacted and worktree arms, which the row-kind check
    // alone could not tell apart from this one, are excluded.
    expect(row.dataset.rowKind).toBe("separation");
    const divider = row.querySelector<HTMLElement>(".separation");
    expect(divider?.dataset.arm).toBe("cleared");
    expect(divider?.querySelector(".sep-accent-cleared")).not.toBeNull();
    expect(textOf(row)).not.toBe("");
  },
  TURN_TEST_MS,
);

it(
  "draws entering and leaving a worktree as separation rows",
  async () => {
    // Arrange
    const before = rows(app, "separation").length;
    // Act
    await family("worktree-keep", "separation");
    // Assert — a worktree episode draws BOTH dividers (entered and left).
    expect(rows(app, "separation").length).toBeGreaterThanOrEqual(before + 2);
    expect(app.$$(".sep-worktree").length).toBeGreaterThan(0);
  },
  TURN_TEST_MS,
);

// §F2 — agent_prompt, and landing 10's delivery outcome on the SENDER's row.
it(
  "names the resumption on the sender's agent-prompt row when the send woke the recipient",
  async () => {
    // Arrange / Act — `!send-message-resumed` sends to an IDLE agent, which the
    // vendor resumes to receive it.
    const row = await family("send-message-resumed", "agentPrompt");

    // Assert — the delivery arm is stated, and it is the resumption.
    expect(row.querySelector("[data-delivery]")?.getAttribute("data-delivery")).toBe(
      "resumedRecipient",
    );
  },
  TURN_TEST_MS,
);

/** Drive a scenario whose turn dies, and read the terminal row it added. */
async function died(scenario: string): Promise<HTMLElement> {
  const before = rows(app, "turnEnded").length;
  await submit(app, `!${scenario}`);
  await awaitDrawn(
    app,
    `the terminal row for !${scenario}`,
    () => rows(app, "turnEnded").length > before,
  );
  const drawn = rows(app, "turnEnded");
  return drawn[drawn.length - 1] as HTMLElement;
}

// §F2 — landing 10: the turn-error line NAMES which way the query died.
it(
  "names an unexpected eof on the turn-error line",
  async () => {
    // Arrange / Act — `!query-eof` ends the agent's stream without a close.
    const row = await died("query-eof");

    // Assert
    expect(row.querySelector("[data-turn-error]")?.getAttribute("data-turn-error")).toBe(
      "queryDied",
    );
    expect(row.querySelector("[data-query-cause]")?.getAttribute("data-query-cause")).toBe(
      "unexpectedEof",
    );
  },
  TURN_TEST_MS,
);

it(
  "names an iterator failure on the turn-error line",
  async () => {
    // Arrange / Act — `!query-fail` throws out of the sdk's iterator.
    const row = await died("query-fail");

    // Assert
    expect(row.querySelector("[data-query-cause]")?.getAttribute("data-query-cause")).toBe(
      "iteratorFailure",
    );
  },
  TURN_TEST_MS,
);
