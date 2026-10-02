/**
 * REFUSALS — the `<Method>Error` arms, drawn AT THE CALL SITE.
 *
 * A refusal is the submitter's own: it belongs to the control that was
 * clicked, never to pushed state, and it never becomes a feed row. That is the
 * rule this file exists to hold, plus the three shapes a refusal can take:
 *
 *   - a TYPED arm (SubmitPrompt.merging, Interrupt.confirm_required,
 *     CloseWorkspace.blocked) drawn with its own words,
 *   - an EMPTY error message, which most methods still are today: the client
 *     has nothing to say but the rpc's name, and must say that rather than
 *     nothing,
 *   - a TRANSPORT failure, which is not a refusal at all but lands in the same
 *     place because that is where the user clicked.
 *
 * And the one thing that is NOT a refusal: a response whose `result` oneof is
 * unset is a malformed view.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";

import { SubmitPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { InterruptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { CloseWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_workspace_pb";
import { SetModelResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_model_pb";
import { AnswerPermissionResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_permission_pb";
import { UpdateHeldPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";

import { bootColdOnce, chipFailureArms, startHarness, type Harness } from "./harness";
import { ROOT_FEED, REFUSAL_FACTS, refusalArmsOf, type RpcName } from "./fake-daemon";
import {
  ACCOUNT_CONFIG_DIR,
  REFUSED_COMMAND,
  WORKSPACE_ID,
  activityRow,
  artifactUnit,
  subagentUnit,
  coldGateStandingRow,
  commandRefusedRow,
  feedId,
  feedPageSuccess,
  footerView,
  heldOfferItem,
  heldPromptItem,
  holdTray,
  permissionRow,
  planUnit,
  questionRow,
  roster,
  rosterRow,
  topbarView,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

describe("SubmitPrompt refused while merging", () => {
  it("draws the refusal at the composer", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: { case: "error", value: { reason: { case: "merging", value: {} } } },
      }),
    );
    const input = harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement;
    input.value = "a prompt mid-merge";
    input.dispatchEvent(new Event("input", { bubbles: true }));
    // Act
    await harness.click("[data-composer-send]");
    // Assert
    expect(harness.$(".composer-refusal[data-arm]")?.dataset.arm).toBe("merging");
  });

  it("preserves the submitted text so nothing is lost", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: { case: "error", value: { reason: { case: "merging", value: {} } } },
      }),
    );
    const input = harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement;
    input.value = "a prompt mid-merge";
    input.dispatchEvent(new Event("input", { bubbles: true }));
    // Act
    await harness.click("[data-composer-send]");
    // Assert
    expect((harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement).value).toBe(
      "a prompt mid-merge",
    );
  });

  it("draws the refusal nowhere in the feed", async () => {
    // Arrange
    harness = await startHarness({ composer: true });
    harness.fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: { case: "error", value: { reason: { case: "merging", value: {} } } },
      }),
    );
    const input = harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement;
    input.value = "a prompt";
    input.dispatchEvent(new Event("input", { bubbles: true }));
    // Act
    await harness.click("[data-composer-send]");
    // Assert
    expect(harness.feedContainer()?.querySelector(".refusal")).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// A COLD-GATED SESSION REFUSES BY ITS OWN NAME (owner's report, 2026-09-14)
//
// A workspace parked at its cold gate HAS a session — that is why a gate could
// be raised — so `no_session` is the wrong arm and the wrong sentence. The gate
// is answered in the panel, and the refusal says so.
// ---------------------------------------------------------------------------

describe("SubmitPrompt refused by the cold gate", () => {
  /** Boot a composer and send one prompt into a cold-gated workspace. */
  const send = async (): Promise<void> => {
    harness = await startHarness({ composer: true });
    harness.fake.refuse("submitPrompt", "coldGate");
    const input = harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement;
    input.value = "a prompt at the gate";
    input.dispatchEvent(new Event("input", { bubbles: true }));
    await harness.settle();
    await harness.click("[data-composer-send]");
  };

  it("labels the refusal with the cold-gate arm", async () => {
    // Arrange / Act
    await send();
    // Assert
    expect(harness.$(".composer-refusal[data-arm]")?.dataset.arm).toBe("coldGate");
  });

  it("says the session is parked at its gate rather than missing", async () => {
    // Arrange / Act
    await send();
    // Assert
    expect(harness.text(".composer-refusal")).toContain("parked at its cold gate");
  });

  it("preserves the submitted text so nothing is lost", async () => {
    // Arrange / Act
    await send();
    // Assert
    expect((harness.$('[data-component="composer"] textarea') as HTMLTextAreaElement).value).toBe(
      "a prompt at the gate",
    );
  });
});

describe("Interrupt refused for confirmation", () => {
  it("draws the refusal at the clicked control", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "working" })),
    });
    harness.fake.answer(
      "interrupt",
      create(InterruptResponseSchema, {
        result: {
          case: "error",
          value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
        },
      }),
    );
    // Act
    await harness.click("[data-interrupt]");
    // Assert
    expect(harness.$("[data-interrupt] ~ .refusal, .refusal")?.dataset.arm).toBe("confirmRequired");
  });

  it("names the live agent count the daemon reported", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "working" })),
    });
    harness.fake.answer(
      "interrupt",
      create(InterruptResponseSchema, {
        result: {
          case: "error",
          value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
        },
      }),
    );
    // Act
    await harness.click("[data-interrupt]");
    // Assert
    expect(harness.$(".refusal")?.textContent).toContain("3");
  });

  it("resends with confirm_agents set when the confirmation is taken", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "working" })),
    });
    harness.fake.answer(
      "interrupt",
      create(InterruptResponseSchema, {
        result: {
          case: "error",
          value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
        },
      }),
    );
    await harness.click("[data-interrupt]");
    // Act
    await harness.click("[data-interrupt-confirm]");
    // Assert
    const requests = harness.fake.calls<{ confirmAgents: boolean }>("interrupt");
    expect(requests.at(-1)?.confirmAgents).toBe(true);
  });

  it("sends the first interrupt without confirm_agents", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "working" })),
    });
    // Act
    await harness.click("[data-interrupt]");
    // Assert
    const [request] = harness.fake.calls<{ confirmAgents: boolean }>("interrupt");
    expect(request.confirmAgents).toBe(false);
  });
});

describe("CloseWorkspace refused as blocked", () => {
  it("draws the refusal at the sidebar's own control", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
    });
    harness.fake.answer(
      "closeWorkspace",
      create(CloseWorkspaceResponseSchema, {
        result: { case: "error", value: { cause: { case: "blocked", value: {} } } },
      }),
    );
    // Act
    await harness.click(`[data-roster-row="${WORKSPACE_ID}"] [data-verb="close"]`);
    // Assert
    expect(
      harness.$(`[data-roster-row="${WORKSPACE_ID}"] .refusal[data-arm]`)?.dataset.arm,
    ).toBe("blocked");
  });

  it("leaves the row drawn rather than removing it", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
    });
    harness.fake.answer(
      "closeWorkspace",
      create(CloseWorkspaceResponseSchema, {
        result: { case: "error", value: { cause: { case: "blocked", value: {} } } },
      }),
    );
    // Act
    await harness.click(`[data-roster-row="${WORKSPACE_ID}"] [data-verb="close"]`);
    // Assert
    expect(harness.$(`[data-roster-row="${WORKSPACE_ID}"]`)).not.toBeNull();
  });
});

/**
 * An `<Method>Error` whose CAUSE IS UNSET.
 *
 * Landing 4 typed every error: each carries a `cause` oneof with the four
 * cross-cutting arms plus its own. So an error with no cause set is not "an
 * empty error the daemon had no words for" — it is a frame this build cannot
 * read, and the ruling on unset oneofs applies to it exactly as it applies to
 * an unset `result`: a MALFORMED VIEW, reported as `frameUndecodable`, with no
 * refusal invented at the control. (This table previously asserted the
 * pre-landing-4 behaviour, when these messages really were empty.)
 */
const UNSET_CAUSE_CASES = [
  {
    name: "SetModel",
    rpc: "setModel" as const,
    response: () =>
      create(SetModelResponseSchema, { result: { case: "error", value: {} } }),
    arrange: (h: Harness) => h.fake.setTopbar(WORKSPACE_ID, topbarView()),
    // The options live in the picker's reveal, so the picker is opened first —
    // the same step the REFUSAL_SITES table below takes for this control.
    before: async (h: Harness) => h.click(".topbar-model"),
    click: '[data-model-option="sonnet"]',
    site: ".topbar-model",
  },
  {
    name: "AnswerPermission",
    rpc: "answerPermission" as const,
    response: () =>
      create(AnswerPermissionResponseSchema, { result: { case: "error", value: {} } }),
    arrange: (h: Harness) =>
      h.fake.pushRow(WORKSPACE_ID, ROOT_FEED, permissionRow("open", undefined, { id: feedId("perm") })),
    before: undefined,
    click: '[data-permission="allowOnce"]',
    site: '[data-feed-row="perm"]',
  },
  {
    name: "UpdateHeldPrompt",
    rpc: "updateHeldPrompt" as const,
    response: () =>
      create(UpdateHeldPromptResponseSchema, { result: { case: "error", value: {} } }),
    arrange: (h: Harness) => h.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem()] })),
    before: undefined,
    click: "[data-held-action='drop']",
    site: '[data-component="hold-tray"]',
  },
];

describe.each(UNSET_CAUSE_CASES)("$name's unset error cause", (testCase) => {
  it("reports a malformed view rather than inventing a refusal", async () => {
    // Arrange
    harness = await startHarness();
    harness.fake.answer(testCase.rpc, testCase.response());
    testCase.arrange(harness);
    await harness.settle();
    await testCase.before?.(harness);
    // Act
    await harness.click(testCase.click);
    // Assert
    expect(harness.failureArms()).toContain("frameUndecodable");
  });

  it("draws no refusal at the call site", async () => {
    // Arrange
    harness = await startHarness();
    harness.fake.answer(testCase.rpc, testCase.response());
    testCase.arrange(harness);
    await harness.settle();
    await testCase.before?.(harness);
    // Act
    await harness.click(testCase.click);
    // Assert
    expect(harness.$(`${testCase.site} .refusal`)).toBeNull();
  });
});

describe("a response with no result arm", () => {
  it("is a malformed view rather than a refusal", async () => {
    // Arrange: `result` unset entirely — neither success nor error.
    harness = await startHarness({
      arrange: (fake) => fake.setTopbar(WORKSPACE_ID, topbarView()),
    });
    harness.fake.answer("setModel", create(SetModelResponseSchema, {}));
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    expect(harness.failureArms()).toContain("frameUndecodable");
  });

  it("draws no refusal for a malformed response", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setTopbar(WORKSPACE_ID, topbarView()),
    });
    harness.fake.answer("setModel", create(SetModelResponseSchema, {}));
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert: a malformed frame is a machinery fault, not the daemon refusing.
    expect(harness.refusalArms()).toEqual([]);
  });
});

describe("a transport failure on a unary call", () => {
  it("draws a refusal at the call site", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setTopbar(WORKSPACE_ID, topbarView()),
    });
    harness.fake.failNext("setModel", "the daemon dropped the call");
    await harness.click(".topbar-model");
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    expect(harness.$(".topbar-model .refusal")).not.toBeNull();
  });

  it("recovers on the next click", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setTopbar(WORKSPACE_ID, topbarView()),
    });
    harness.fake.failNext("setModel", "the daemon dropped the call");
    await harness.click(".topbar-model");
    await harness.click('[data-model-option="sonnet"]');
    // Act
    await harness.click('[data-model-option="sonnet"]');
    // Assert
    expect(harness.$(".topbar-model .refusal")).toBeNull();
  });
});

describe("RequestCommandSupport", () => {
  it("echoes the refused card's own command text", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, commandRefusedRow({ command: REFUSED_COMMAND }));
    await harness.settle();
    // Act
    await harness.click("[data-add-support]");
    // Assert
    const [request] = harness.fake.calls<{ command: string }>("requestCommandSupport");
    expect(request.command).toBe(REFUSED_COMMAND);
  });

  it("draws only a note on success, never a new surface", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, commandRefusedRow());
    await harness.settle();
    // Act
    await harness.click("[data-add-support]");
    // Assert: the roster shows the new workspace; the card just acknowledges.
    expect(harness.row("row-1")?.querySelector("[data-support-note]")).not.toBeNull();
  });

  it("draws its refusal at the add-support button", async () => {
    // Arrange: a TYPED cause. Since landing 4 an error with no cause set is a
    // frame this build cannot read, not a refusal (see the unset-cause table).
    harness = await startHarness();
    harness.fake.refuse("requestCommandSupport", "unknownWorkspace");
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, commandRefusedRow());
    await harness.settle();
    // Act
    await harness.click("[data-add-support]");
    // Assert
    expect(harness.row("row-1")?.querySelector(".refusal")).not.toBeNull();
  });
});

describe("OpenInEditor", () => {
  it("draws its refusal at the link that was clicked", async () => {
    // Arrange
    harness = await startHarness();
    // A TYPED cause: an unset one is an unreadable frame, not a refusal.
    harness.fake.refuse("openInEditor", "unknownWorkspace");
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(planUnit("planned")));
    await harness.settle();
    // Act
    await harness.click("[data-editor-link]");
    // Assert
    expect(harness.row("row-1")?.querySelector(".refusal")).not.toBeNull();
  });

  it("draws nothing at all on success", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(planUnit("planned")));
    await harness.settle();
    const before = harness.row("row-1")?.innerHTML;
    // Act
    await harness.click("[data-editor-link]");
    // Assert
    expect(harness.row("row-1")?.innerHTML).toBe(before);
  });
});

describe("a refusal is never pushed state", () => {
  it("clears when the row it belongs to is re-pushed", async () => {
    // Arrange
    harness = await startHarness();
    harness.fake.answer(
      "answerPermission",
      create(AnswerPermissionResponseSchema, { result: { case: "error", value: {} } }),
    );
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, permissionRow("open", undefined, { id: feedId("perm") }));
    await harness.settle();
    await harness.click('[data-permission="allowOnce"]');
    // Act: the daemon's own view of the card arrives.
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, permissionRow("allowedOnce", undefined, { id: feedId("perm") }));
    await harness.settle();
    // Assert
    expect(harness.row("perm")?.querySelector(".refusal")).toBeNull();
  });

  it("never survives into a paged history", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([])),
    });
    // Assert: nothing in a cold page carries a refusal.
    expect(harness.refusalArms()).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// TYPED REFUSAL ARMS (landing 4)
//
// Every `<Rpc>Error` now carries a typed arm oneof: the four cross-cutting
// causes on every per-workspace rpc, plus per-rpc arms. The suite drives them
// off the SCHEMA (`fake.refusalArms(rpc)`), so an arm landed in the contract
// shows up here without anyone remembering to add it.
// ---------------------------------------------------------------------------

/** The four every per-workspace rpc declares. */
const CROSS_CUTTING_ARMS = [
  "unknownWorkspace",
  "workspaceRefMismatch",
  "transferringAway",
  "notYetAdopted",
] as const;

/**
 * How to provoke each rpc the app actually calls, and where its refusal must
 * land. `arrange` scripts the daemon and draws whatever the click needs.
 */
interface RefusalSite {
  name: string;
  rpc: RpcName;
  /** The selector clicked to make the call. */
  click: string;
  /** Where the refusal must be drawn. */
  site: string;
  /** Put the app in a state where `click` exists. */
  arrange?(h: Harness): void | Promise<void>;
  /** Extra steps between arrange and the click. */
  before?(h: Harness): Promise<void>;
}

const REFUSAL_SITES: RefusalSite[] = [
  {
    name: "SubmitPrompt",
    rpc: "submitPrompt",
    click: "[data-composer-send]",
    site: '[data-component="composer"]',
    before: async (h) => {
      const input = h.$('[data-component="composer"] textarea') as HTMLTextAreaElement;
      input.value = "a prompt";
      input.dispatchEvent(new Event("input", { bubbles: true }));
      await h.settle();
    },
  },
  {
    name: "Interrupt",
    rpc: "interrupt",
    click: "[data-interrupt]",
    site: '[data-component="footer"]',
    arrange: (h) => h.fake.setFooter(WORKSPACE_ID, footerView({ status: "working" })),
  },
  {
    name: "AnswerPermission",
    rpc: "answerPermission",
    click: '[data-permission="allowOnce"]',
    site: '[data-feed-row="perm"]',
    arrange: (h) =>
      h.fake.pushRow(WORKSPACE_ID, ROOT_FEED, permissionRow("open", undefined, { id: feedId("perm") })),
  },
  {
    name: "AnswerQuestion",
    rpc: "answerQuestion",
    click: "[data-question-submit]",
    site: '[data-feed-row="ask"]',
    arrange: (h) => h.fake.pushRow(WORKSPACE_ID, ROOT_FEED, questionRow("open", { id: feedId("ask") })),
    // The batch is answered WHOLE (src/feed/asks/question.ts: an unanswered
    // question blocks submit in place rather than sending a partial batch), so
    // every question is answered before the submit that provokes the refusal.
    before: async (h) => {
      for (const option of h.$$('[data-feed-row="ask"] .q-opts input')) option.click();
      await h.settle();
    },
  },
  {
    name: "AnswerColdGate",
    rpc: "answerColdGate",
    click: '[data-cold-gate="pay"]',
    site: '[data-feed-row="gate"]',
    arrange: (h) =>
      h.fake.pushRow(WORKSPACE_ID, ROOT_FEED, coldGateStandingRow(undefined, { id: feedId("gate") })),
  },
  {
    name: "UpdateHeldPrompt",
    rpc: "updateHeldPrompt",
    click: '[data-held-action="drop"]',
    site: '[data-component="hold-tray"]',
    arrange: (h) => h.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem()] })),
  },
  {
    name: "AnswerHeldOffer",
    rpc: "answerHeldOffer",
    click: '[data-offer-decision="keep"]',
    site: '[data-component="hold-tray"]',
    arrange: (h) => h.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldOfferItem()] })),
  },
  {
    name: "CloseWorkspace",
    rpc: "closeWorkspace",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-verb="close"]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "KillWorkspace",
    rpc: "killWorkspace",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-verb="kill"]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "NukeWorkspace",
    rpc: "nukeWorkspace",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-verb="nuke"]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "MergeWorkspace",
    rpc: "mergeWorkspace",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-verb="merge"]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "RestartWorkspace",
    rpc: "restartWorkspace",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-verb="restart"]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "OpenWorkspace",
    rpc: "openWorkspace",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-verb="open"]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "SetWorkspacePriority",
    rpc: "setWorkspacePriority",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-verb="priority"]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "SelectWorkspace",
    rpc: "selectWorkspace",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-select]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "AssignWorkspaceTask",
    rpc: "assignWorkspaceTask",
    click: `[data-roster-row="${WORKSPACE_ID}"] [data-assign-task="task-1"]`,
    site: `[data-roster-row="${WORKSPACE_ID}"]`,
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "SetModel",
    rpc: "setModel",
    click: '[data-model-option="sonnet"]',
    site: ".topbar-model",
    arrange: (h) => h.fake.setTopbar(WORKSPACE_ID, topbarView()),
    before: async (h) => h.click(".topbar-model"),
  },
  {
    name: "SetPermissionMode",
    rpc: "setPermissionMode",
    click: '[data-mode-option="plan"]',
    site: ".topbar-mode",
    arrange: (h) => h.fake.setTopbar(WORKSPACE_ID, topbarView()),
    before: async (h) => h.click(".topbar-mode"),
  },
  {
    // THE PICK IS WHAT OPENS THE LOGIN, and the cell is where its refusal
    // lands. The account cell's own click opens the options dropdown (owner
    // ruling, 2026-09-13, `src/topbar/account.ts`); picking a root with no
    // login succeeds and the client opens OpenLogin behind it, with the CELL
    // as the control it passes — so the refusal draws on
    // `.topbar-account-cell`, the element the reveal hangs under.
    name: "OpenLogin",
    rpc: "openLogin",
    click: `[data-account-option="${ACCOUNT_CONFIG_DIR}"]`,
    site: ".topbar-account-cell",
    arrange: (h) => h.fake.setTopbar(WORKSPACE_ID, topbarView({ account: "loggedOut" })),
    before: async (h) => {
      await h.click(".topbar-account");
    },
  },
  {
    // SelectAccount's own refusals draw at the same cell, for the same reason.
    name: "SelectAccount",
    rpc: "selectAccount",
    click: `[data-account-option="${ACCOUNT_CONFIG_DIR}"]`,
    site: ".topbar-account-cell",
    arrange: (h) => h.fake.setTopbar(WORKSPACE_ID, topbarView({ account: "loggedOut" })),
    before: async (h) => {
      await h.click(".topbar-account");
    },
  },
  {
    name: "OpenExternal",
    rpc: "openExternal",
    click: "[data-external-link]",
    site: '[data-feed-row="row-1"]',
    arrange: (h) => h.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(artifactUnit("published"))),
  },
  {
    name: "OpenInEditor",
    rpc: "openInEditor",
    click: "[data-editor-link]",
    site: '[data-feed-row="row-1"]',
    arrange: (h) => h.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(planUnit("planned"))),
  },
  {
    name: "RestartWorkspace",
    rpc: "restartWorkspace",
    click: '[data-roster-row="ws-1"] [data-verb="restart"]',
    site: '[data-roster-row="ws-1"]',
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
  },
  {
    name: "SelectWorkspace",
    rpc: "selectWorkspace",
    click: '[data-roster-row="ws-other"] [data-select]',
    site: '[data-roster-row="ws-other"]',
    arrange: (h) => h.fake.setRoster(roster({ rows: [rosterRow({ id: "ws-other" })] })),
  },
  {
    name: "SetModel",
    rpc: "setModel",
    click: '[data-model-option="sonnet"]',
    site: ".topbar-model",
    arrange: (h) => h.fake.setTopbar(WORKSPACE_ID, topbarView()),
    before: async (h) => {
      await h.click(".topbar-model");
    },
  },
  {
    name: "SetPermissionMode",
    rpc: "setPermissionMode",
    click: '[data-mode-option="plan"]',
    site: ".topbar-mode",
    arrange: (h) => h.fake.setTopbar(WORKSPACE_ID, topbarView()),
    before: async (h) => {
      await h.click(".topbar-mode");
    },
  },
  {
    name: "RequestCommandSupport",
    rpc: "requestCommandSupport",
    click: "[data-add-support]",
    site: '[data-feed-row="row-1"]',
    arrange: (h) => h.fake.pushRow(WORKSPACE_ID, ROOT_FEED, commandRefusedRow()),
  },
];

/** Boot, script the refusal, put the app in reach of the control, and click. */
async function provoke(testCase: RefusalSite, arm: string): Promise<Harness> {
  const h = await startHarness({ composer: true });
  await h.fake.awaitStream("watchFeed");
  h.fake.refuse(testCase.rpc, arm);
  await testCase.arrange?.(h);
  await h.settle();
  await testCase.before?.(h);
  await h.click(testCase.click);
  return h;
}

describe.each(REFUSAL_SITES)("$name's typed refusals", (testCase) => {
  it("declares the four cross-cutting arms on its error", () => {
    // Assert: read off the descriptor, so a dropped arm fails here.
    expect(refusalArmsOf(testCase.rpc)).toEqual(expect.arrayContaining([...CROSS_CUTTING_ARMS]));
  });

  it.each(CROSS_CUTTING_ARMS)("draws the %s arm at its call site", async (arm) => {
    // Arrange / Act
    harness = await provoke(testCase, arm);
    // Assert
    expect(harness.$(`${testCase.site} .refusal[data-arm="${arm}"]`)).not.toBeNull();
  });

  it("draws the registry dir the ref-mismatch arm carries", async () => {
    // Arrange / Act
    harness = await provoke(testCase, "workspaceRefMismatch");
    // Assert
    expect(harness.$(`${testCase.site} .refusal`)?.textContent).toContain(REFUSAL_FACTS.registryDir);
  });

  it("draws the address the transferring-away arm carries", async () => {
    // Arrange / Act
    harness = await provoke(testCase, "transferringAway");
    // Assert
    expect(harness.$(`${testCase.site} .refusal`)?.textContent).toContain(REFUSAL_FACTS.address);
  });

  it("draws no refusal in the feed for a control outside it", async () => {
    // Arrange / Act
    harness = await provoke(testCase, "unknownWorkspace");
    // Assert: exactly one refusal stands, and it is the one at the call site.
    expect(harness.refusalArms()).toEqual(["unknownWorkspace"]);
  });
});

describe("per-rpc refusal arms", () => {
  /** Every arm that is NOT one of the four cross-cutting ones. */
  const perRpcArms = (rpc: RpcName): string[] =>
    refusalArmsOf(rpc).filter((a) => !(CROSS_CUTTING_ARMS as readonly string[]).includes(a));

  it.each(
    REFUSAL_SITES.flatMap((testCase) =>
      perRpcArms(testCase.rpc)
        // `link_unresolved` is drawn nowhere by this page: the daemon publishes
        // the footer line and asks the question in the conversation.
        .filter((arm) => arm !== "linkUnresolved")
        .map((arm) => ({ name: testCase.name, testCase, arm })),
    ),
  )("draws $name's $arm arm at its call site", async ({ testCase, arm }) => {
    // Arrange / Act
    harness = await provoke(testCase, arm);
    // Assert
    expect(harness.$(`${testCase.site} .refusal[data-arm="${arm}"]`)).not.toBeNull();
  });
});

describe("OpenInEditor's link_unresolved arm", () => {
  it("draws nothing at the link, the daemon speaking for it", async () => {
    // Arrange / Act
    harness = await provoke(REFUSAL_SITES.find((s) => s.rpc === "openInEditor")!, "linkUnresolved");
    // Assert
    expect(harness.$('[data-feed-row="row-1"] .refusal')).toBeNull();
  });
});

describe("AdoptWebWorkspace's refusals", () => {
  /**
   * ADOPTION HAPPENS ONCE, AT BOOT, on the daemon the page was addressed to
   * (project lead's CONFIRMED SEQUENCE in the lifecycle brief). This block used
   * to arrange a SECOND daemon and refuse the adoption there, which was the
   * retired redial path — the webapp never dials a successor.
   *
   * The refusal has no clicked control, so it is machinery failing rather than
   * a user's click: `controlPlaneFailed` is filed, and the boot throws so no
   * view is ever mounted over a workspace this page could not adopt. The
   * `notYetAdopted` arm is the RETRY arm and never reaches this path; its
   * backoff and its budget are covered under fake timers in
   * test/lifecycle/lifecycle.test.ts.
   */
  const TERMINAL_ARMS = CROSS_CUTTING_ARMS.filter((arm) => arm !== "notYetAdopted");

  it.each(TERMINAL_ARMS)("reports the %s arm as a client-local failure", async (arm) => {
    // Arrange / Act
    await expect(
      startHarness({ arrange: (fake) => fake.refuse("adoptWebWorkspace", arm) }),
    ).rejects.toThrow();
    // Assert
    expect(chipFailureArms()).toContain("controlPlaneFailed");
  });

  it.each(TERMINAL_ARMS)("fails the boot on the %s arm", async (arm) => {
    // Arrange / Act
    await expect(
      startHarness({ arrange: (fake) => fake.refuse("adoptWebWorkspace", arm) }),
    ).rejects.toThrow();
    // Assert
    expect(chipFailureArms()).toContain("bootFailed");
  });
});

// ---------------------------------------------------------------------------
// A BUBBLE COMPOSER'S REFUSAL IS THE BUBBLE'S (audit 1, item 8)
//
// "EVERY CLICK IS AN RPC, and its refusal renders AT the clicked control." A
// bubble composer is a control inside a row, so its refusal belongs inside that
// row — drawing it at the root composer would tell a reader their ROOT prompt
// was refused, which is a different fact about a different submission.
// ---------------------------------------------------------------------------

describe("a bubble composer's refusal", () => {
  /** Expand a bubble, type into ITS box, and send against a refusing daemon. */
  const sendFromBubble = async (arm: string): Promise<void> => {
    harness = await startHarness({
      composer: true,
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    harness.fake.refuse("submitPrompt", arm);
    const input = harness.$('[data-feed-row="bubble"] textarea') as HTMLTextAreaElement;
    input.value = "into the bubble";
    input.dispatchEvent(new Event("input", { bubbles: true }));
    await harness.settle();
    await harness.click('[data-feed-row="bubble"] [data-composer-send]');
  };

  it("draws the refusal inside the bubble", async () => {
    // Arrange / Act
    await sendFromBubble("noSession");
    // Assert
    expect(harness.$('[data-feed-row="bubble"] .composer-refusal')).not.toBeNull();
  });

  it("draws no refusal at the root composer", async () => {
    // Arrange / Act
    await sendFromBubble("noSession");
    // Assert
    expect(harness.shell.composer.querySelector(":scope > .composer-box .composer-refusal")).toBeNull();
  });

  it("draws exactly one refusal on the page", async () => {
    // Arrange / Act
    await sendFromBubble("noSession");
    // Assert
    expect(harness.refusalArms()).toEqual(["noSession"]);
  });

  it("keeps the bubble's own text", async () => {
    // Arrange / Act
    await sendFromBubble("noSession");
    // Assert
    expect((harness.$('[data-feed-row="bubble"] textarea') as HTMLTextAreaElement).value).toBe(
      "into the bubble",
    );
  });

  it("draws a cross-cutting arm inside the bubble too", async () => {
    // Arrange / Act
    await sendFromBubble("unknownWorkspace");
    // Assert
    expect(
      harness.$('[data-feed-row="bubble"] .composer-refusal[data-arm="unknownWorkspace"]'),
    ).not.toBeNull();
  });
});

// ---------------------------------------------------------------------------
// THE CREATE FORM AND THE TASK VERBS (audit 1, item 9)
//
// CreateWorkspace is addressed to a REPOSITORY rather than to an existing
// workspace, so none of the cross-cutting four can reach it: every arm is the
// endpoint's own, and this table IS the whole vocabulary. The same is true of
// CreateTask and UpdateTask, which are addressed to a task.
// ---------------------------------------------------------------------------

describe("the create-workspace form", () => {
  /** Open the "+" form in the repository section. */
  const openForm = async (refusal?: string): Promise<void> => {
    harness = await startHarness({
      arrange: (fake) => fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
    });
    if (refusal !== undefined) harness.fake.refuse("createWorkspace", refusal);
    await harness.click(".sb-add");
  };

  it("declares none of the cross-cutting four", () => {
    // Assert: a creation names no existing workspace, so it cannot be refused
    // for one — read off the descriptor so a landed arm shows up here.
    const shared = refusalArmsOf("createWorkspace").filter((arm) =>
      (CROSS_CUTTING_ARMS as readonly string[]).includes(arm),
    );
    expect(shared).toEqual([]);
  });

  it("echoes the section's own repository", async () => {
    // Arrange
    await openForm();
    // Act
    await harness.click("[data-create-submit]");
    // Assert
    const [request] = harness.fake.calls<{ repository?: { id: string } }>("createWorkspace");
    expect(request.repository?.id).toBe("repo-1");
  });

  it("sends the standard form by default", async () => {
    // Arrange
    await openForm();
    // Act
    await harness.click("[data-create-submit]");
    // Assert
    const [request] = harness.fake.calls<{ form: { case?: string } }>("createWorkspace");
    expect(request.form.case).toBe("standard");
  });

  it("carries a typed field into the request", async () => {
    // Arrange
    await openForm();
    const name = harness.$('[data-create-form] [name="name"]') as HTMLInputElement;
    name.value = "audit-1";
    name.dispatchEvent(new Event("input", { bubbles: true }));
    // Act
    await harness.click("[data-create-submit]");
    // Assert
    const [request] = harness.fake.calls<{ form: { value?: { name?: string } } }>("createWorkspace");
    expect(request.form.value?.name).toBe("audit-1");
  });

  it("sends the one-shot form when that mode is picked", async () => {
    // Arrange
    await openForm();
    const prompt = harness.$('[data-create-form] [name="one_shot_prompt"]') as HTMLTextAreaElement;
    prompt.value = "do the thing";
    prompt.dispatchEvent(new Event("input", { bubbles: true }));
    await harness.click('[data-create-form] [name="form"][value="one_shot"]');
    // Act
    await harness.click("[data-create-submit]");
    // Assert
    const [request] = harness.fake.calls<{ form: { case?: string } }>("createWorkspace");
    expect(request.form.case).toBe("oneShot");
  });

  it.each(refusalArmsOf("createWorkspace"))("draws the %s arm at the form", async (arm) => {
    // Arrange
    await openForm(arm);
    // Act
    await harness.click("[data-create-submit]");
    // Assert
    expect(harness.$(`[data-create-form] .refusal[data-arm="${arm}"]`)).not.toBeNull();
  });

  it("names the unresolved base ref the arm carries", async () => {
    // Arrange
    await openForm("baseRefUnresolved");
    // Act
    await harness.click("[data-create-submit]");
    // Assert
    expect(harness.$("[data-create-form] .refusal")?.textContent).toContain(REFUSAL_FACTS.ref);
  });

  it("keeps the form open on a refusal", async () => {
    // Arrange: the form's next move depends on which arm it was, so it stays.
    await openForm("noSlug");
    // Act
    await harness.click("[data-create-submit]");
    // Assert
    expect(harness.$("[data-create-form]")).not.toBeNull();
  });

  it("closes the form on a success", async () => {
    // Arrange: the new row arrives on the roster push.
    await openForm();
    // Act
    await harness.click("[data-create-submit]");
    // Assert
    expect(harness.$("[data-create-form]")).toBeNull();
  });
});

describe("the task verbs' refusals", () => {
  /** Boot with a roster and a refusing task verb. */
  const withRefusal = async (rpc: RpcName, arm: string): Promise<void> => {
    harness = await startHarness({
      arrange: (fake) => fake.setRoster(roster({ rows: [rosterRow({ id: WORKSPACE_ID })] })),
    });
    harness.fake.refuse(rpc, arm);
  };

  /**
   * Type a title into the new-task form.
   *
   * A BLANK TITLE IS NEVER SENT (src/sidebar/tasks.ts: the contract says the
   * title is non-blank, so the refusal is avoided rather than provoked), so a
   * test that clicked with an empty box would provoke no rpc at all and
   * observe no refusal for the wrong reason.
   */
  const typeTitle = async (title: string): Promise<void> => {
    const input = harness.$("[data-task-title]") as HTMLInputElement;
    input.value = title;
    input.dispatchEvent(new Event("input", { bubbles: true }));
    await harness.settle();
  };

  it.each(refusalArmsOf("createTask"))("draws CreateTask's %s arm at its control", async (arm) => {
    // Arrange
    await withRefusal("createTask", arm);
    await typeTitle("a new task");
    // Act
    await harness.click("[data-task-create]");
    // Assert
    expect(harness.$(`[data-component="sidebar"] .refusal[data-arm="${arm}"]`)).not.toBeNull();
  });

  it.each(refusalArmsOf("updateTask"))("draws UpdateTask's %s arm at its control", async (arm) => {
    // Arrange
    await withRefusal("updateTask", arm);
    // Act
    await harness.click('[data-task-change="setDone"]');
    // Assert
    expect(harness.$(`[data-component="sidebar"] .refusal[data-arm="${arm}"]`)).not.toBeNull();
  });

  it("draws exactly one refusal for a refused task create", async () => {
    // Arrange
    await withRefusal("createTask", "blankTitle");
    await typeTitle("a new task");
    // Act
    await harness.click("[data-task-create]");
    // Assert
    expect(harness.refusalArms()).toEqual(["blankTitle"]);
  });

  it("draws no task refusal in the feed", async () => {
    // Arrange
    await withRefusal("updateTask", "unknownTask");
    // Act
    await harness.click('[data-task-change="setDone"]');
    // Assert
    expect(harness.feedContainer()?.querySelector(".refusal")).toBeNull();
  });
});
