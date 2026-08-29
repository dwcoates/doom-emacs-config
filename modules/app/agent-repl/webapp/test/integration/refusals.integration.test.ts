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
import { OpenInEditorResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import { RequestCommandSupportResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_request_command_support_pb";
import { UpdateHeldPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";

import { startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import {
  REFUSED_COMMAND,
  WORKSPACE_ID,
  activityRow,
  commandRefusedRow,
  feedId,
  feedPageSuccess,
  footerView,
  heldPromptItem,
  holdTray,
  permissionRow,
  planUnit,
  roster,
  rosterRow,
  topbarView,
} from "./fixtures";

let harness: Harness;

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

describe("Interrupt refused for confirmation", () => {
  it("draws the refusal at the clicked control", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" })),
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
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" })),
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
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" })),
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
      arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView({ status: "thinking" })),
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
 * Most `<Method>Error` messages are still EMPTY today. An empty error is not
 * "no error": the click failed, and the user is entitled to know which verb
 * failed even when the daemon offered no words.
 */
const EMPTY_ERROR_CASES = [
  {
    name: "SetModel",
    rpc: "setModel" as const,
    response: () =>
      create(SetModelResponseSchema, { result: { case: "error", value: {} } }),
    arrange: (h: Harness) => h.fake.setTopbar(WORKSPACE_ID, topbarView()),
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
    click: '[data-permission="allowOnce"]',
    site: '[data-feed-row="perm"]',
  },
  {
    name: "UpdateHeldPrompt",
    rpc: "updateHeldPrompt" as const,
    response: () =>
      create(UpdateHeldPromptResponseSchema, { result: { case: "error", value: {} } }),
    arrange: (h: Harness) => h.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem()] })),
    click: "[data-held-action='drop']",
    site: '[data-component="hold-tray"]',
  },
];

describe.each(EMPTY_ERROR_CASES)("$name's empty error", (testCase) => {
  it("draws a refusal naming the rpc rather than nothing", async () => {
    // Arrange
    harness = await startHarness();
    harness.fake.answer(testCase.rpc, testCase.response());
    testCase.arrange(harness);
    await harness.settle();
    // Act
    await harness.click(testCase.click);
    // Assert
    expect(harness.$(`${testCase.site} .refusal`)?.textContent?.toLowerCase()).toContain(
      testCase.rpc.toLowerCase(),
    );
  });

  it("draws the refusal at the call site", async () => {
    // Arrange
    harness = await startHarness();
    harness.fake.answer(testCase.rpc, testCase.response());
    testCase.arrange(harness);
    await harness.settle();
    // Act
    await harness.click(testCase.click);
    // Assert
    expect(harness.$(`${testCase.site} .refusal`)).not.toBeNull();
  });
});

describe("a response with no result arm", () => {
  it("is a malformed view rather than a refusal", async () => {
    // Arrange: `result` unset entirely — neither success nor error.
    harness = await startHarness({
      arrange: (fake) => fake.setTopbar(WORKSPACE_ID, topbarView()),
    });
    harness.fake.answer("setModel", create(SetModelResponseSchema, {}));
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
    // Arrange
    harness = await startHarness();
    harness.fake.answer(
      "requestCommandSupport",
      create(RequestCommandSupportResponseSchema, { result: { case: "error", value: {} } }),
    );
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
    harness.fake.answer(
      "openInEditor",
      create(OpenInEditorResponseSchema, { result: { case: "error", value: {} } }),
    );
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
