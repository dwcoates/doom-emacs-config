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
 * TWO FAMILIES ARE ABSENT AND SAID SO, rather than covered with an invented
 * fixture: `agent_prompt` and `cold_gate`. No fake-SDK scenario any Go area
 * test drives produces either row — `send-message`, `wakeup-schedule` and
 * `cron` all draw a plain response, and no Go e2e test references the cold
 * gate at all. Both are reported to the fake-SDK owner; when a scenario
 * lands, each becomes one more `it` here.
 *
 * ONE PAGE, ONE WORKSPACE, MANY TURNS. The page is mounted once for the file
 * (the Go driver hands this child exactly one world), so rows accumulate as
 * the file runs and each test reads the NEWEST row of its family. Each test
 * also waits out its own turn, so the next submission is never queued behind
 * a running one.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import { BOOT_BUDGET_MS, TURN_TEST_MS, bootLayer, driveTurn, rows, textOf } from "./drive";

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
  "draws a web-fetch tool call's links as link rows",
  async () => {
    // Arrange / Act
    const row = await family("web-fetch", "activity", "simpleToolCall");
    // Assert — the links output form, not a text blob.
    expect(row.querySelector(".tool-head")).not.toBeNull();
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
    const card = row.querySelector<HTMLElement>("[data-state]");
    expect(card).not.toBeNull();
    expect(card?.dataset.state).not.toBe("");
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
    // Assert
    expect(row.dataset.unit).toBe("hook");
    expect(textOf(row)).not.toBe("");
  },
  TURN_TEST_MS,
);

it(
  "draws a failed hook's card",
  async () => {
    // Arrange / Act
    const row = await family("hook-failed", "activity", "hook");
    // Assert
    expect(row.dataset.unit).toBe("hook");
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
    // Assert
    expect(row.dataset.rowKind).toBe("separation");
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
