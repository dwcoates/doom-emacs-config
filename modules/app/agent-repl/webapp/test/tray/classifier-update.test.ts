// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  ClassifierRoute,
  UpdateClassifierPromptErrorSchema,
  UpdateClassifierPromptResponseSchema,
  type UpdateClassifierPromptRequest,
  type UpdateClassifierPromptResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_classifier_prompt_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { testAppContext } from "../rpc/app-context.js";
import { createTicker } from "../../src/clock.js";
import {
  CLASSIFIER_FORM_CLASS,
  ClassifierUpdateForms,
  UPDATE_CLASSIFIER_PLACEHOLDER,
  UPDATE_CLASSIFIER_REFUSALS,
  classifierRouteOf,
  updatedSentence,
  type ClassifierExample,
  type ClassifierUpdateForm,
} from "../../src/tray/classifier-update.js";
import type { Control } from "../../src/control.js";
import { oneofArms } from "../arms.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { resetLoggingForTests } from "../../src/log.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const EXAMPLE: ClassifierExample = { text: "after the tests pass, bump the version", route: ClassifierRoute.HOLD_FOR_TURN_END };

/** The fill each refusal arm's own fields take. */
const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  uncommittedChanges: { path: "/p/queue-routing-classifier.md" },
  rewriteFailed: { detail: "timeout" },
  rewriteRejected: { detail: "a slot was dropped" },
  commitFailed: { detail: "REFUSED" },
};

const success = (): UpdateClassifierPromptResponse =>
  create(UpdateClassifierPromptResponseSchema, {
    result: { case: "success", value: { commit: "0123456789abcdef", path: "/p/queue-routing-classifier.md" } },
  });

const refusal = (arm: string) => (): UpdateClassifierPromptResponse =>
  create(UpdateClassifierPromptResponseSchema, {
    result: { case: "error", value: { cause: { case: arm, value: CAUSE_FILL[arm] ?? {} } } },
  } as never);

/** A registry whose UpdateClassifierPrompt answers with ANSWER and records requests. */
function forms(answer: () => UpdateClassifierPromptResponse | Promise<UpdateClassifierPromptResponse> = success): {
  registry: ClassifierUpdateForms;
  seen: UpdateClassifierPromptRequest[];
  reported: FailureKind[];
} {
  const seen: UpdateClassifierPromptRequest[] = [];
  const reported: FailureKind[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      updateClassifierPrompt: (request) => {
        seen.push(request);
        return answer();
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: createTicker(60_000),
    failures: { report: (kind) => reported.push(kind), retract: () => undefined },
    composerEnabled: false,
  });
  return { registry: new ClassifierUpdateForms(ctx), seen, reported };
}

/** The form's parts, by their hooks. */
function parts(form: ClassifierUpdateForm): {
  input: HTMLTextAreaElement;
  apply: Control;
  dismiss: Control;
  status: HTMLElement;
} {
  const el = form.element;
  return {
    input: el.querySelector<HTMLTextAreaElement>("textarea")!,
    apply: el.querySelector<Control>('[data-classifier-action="apply"]')!,
    dismiss: el.querySelector<Control>('[data-classifier-action="dismiss"]')!,
    status: el.querySelector<HTMLElement>(".classifier-update-status")!,
  };
}

/** Type TEXT into the form's input as a reader would. */
function type(form: ClassifierUpdateForm, text: string): void {
  const { input } = parts(form);
  input.value = text;
  input.dispatchEvent(new Event("input"));
}

/** A form attached to the document, as a drawn card's is. */
function attached(form: ClassifierUpdateForm): ClassifierUpdateForm {
  document.body.appendChild(form.element);
  return form;
}

/** Let the click's promise chain settle. */
const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

afterEach(() => {
  document.body.replaceChildren();
  resetLoggingForTests();
});

describe("classifierRouteOf", () => {
  it.each([
    ["interject", ClassifierRoute.INTERRUPT],
    ["afterToolCall", ClassifierRoute.AFTER_TOOL_CALL],
    ["holdForTurnEnd", ClassifierRoute.HOLD_FOR_TURN_END],
  ] as const)("reads the %s verdict as its route", (arm, route) => {
    // Act / Assert
    expect(classifierRouteOf(arm)).toBe(route);
  });

  it.each(["classifying", "uninterruptibleTurn", "classificationError", "daemonHeld"] as const)(
    "answers null for %s, which no classifier decided",
    (arm) => {
      // Act / Assert
      expect(classifierRouteOf(arm)).toBeNull();
    },
  );
});

describe("ClassifierUpdateForms", () => {
  it("hands back the same form for the same turn", () => {
    // Arrange
    const { registry } = forms();
    // Act
    const first = registry.formFor("turn-1", EXAMPLE);
    const second = registry.formFor("turn-1", EXAMPLE);
    // Assert
    expect(second).toBe(first);
  });

  it("gives each turn a form of its own, stated by its data-says", () => {
    // Arrange
    const { registry } = forms();
    // Act
    const one = registry.formFor("turn-1", EXAMPLE).element;
    const two = registry.formFor("turn-2", EXAMPLE).element;
    // Assert
    expect([one.getAttribute("data-says"), two.getAttribute("data-says")]).toEqual([
      `${CLASSIFIER_FORM_CLASS}:turn-1`,
      `${CLASSIFIER_FORM_CLASS}:turn-2`,
    ]);
  });

  it("drops the form of a turn no longer held", () => {
    // Arrange
    const { registry } = forms();
    registry.formFor("turn-1", EXAMPLE);
    registry.formFor("turn-2", EXAMPLE);
    // Act
    registry.retain(["turn-2"]);
    // Assert
    expect(registry.turns()).toEqual(["turn-2"]);
  });

  it("asks from the example the latest drawing stated", async () => {
    // Arrange
    const { registry, seen } = forms();
    const form = attached(registry.formFor("turn-1", EXAMPLE));
    registry.formFor("turn-1", { text: "stop and rebase", route: ClassifierRoute.INTERRUPT });
    type(form, "interrupt for rebases");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    expect([seen[0]?.example?.text, seen[0]?.example?.route]).toEqual(["stop and rebase", ClassifierRoute.INTERRUPT]);
  });
});

describe("the classifier update form", () => {
  it("starts hidden", () => {
    // Arrange
    const { registry } = forms();
    // Act
    const form = registry.formFor("turn-1", EXAMPLE);
    // Assert
    expect(form.element.hidden).toBe(true);
  });

  it("opens on toggle and focuses its input", () => {
    // Arrange
    const form = attached(forms().registry.formFor("turn-1", EXAMPLE));
    // Act
    form.toggle();
    // Assert
    expect([form.element.hidden, document.activeElement === parts(form).input]).toEqual([false, true]);
  });

  it("closes on a second toggle", () => {
    // Arrange
    const form = forms().registry.formFor("turn-1", EXAMPLE);
    form.toggle();
    // Act
    form.toggle();
    // Assert
    expect(form.element.hidden).toBe(true);
  });

  it("asks for the change in its placeholder", () => {
    // Arrange
    const form = forms().registry.formFor("turn-1", EXAMPLE);
    // Act / Assert
    expect(parts(form).input.placeholder).toBe(UPDATE_CLASSIFIER_PLACEHOLDER);
  });

  it("offers no apply while the input is blank", () => {
    // Arrange
    const form = forms().registry.formFor("turn-1", EXAMPLE);
    // Act
    type(form, "   ");
    // Assert
    expect(parts(form).apply.disabled).toBe(true);
  });

  it("offers apply once the input says something", () => {
    // Arrange
    const form = forms().registry.formFor("turn-1", EXAMPLE);
    // Act
    type(form, "interrupt for 'after'");
    // Assert
    expect(parts(form).apply.disabled).toBe(false);
  });

  it("sends the trimmed instruction and the card's example", async () => {
    // Arrange
    const { registry, seen } = forms();
    const form = attached(registry.formFor("turn-1", EXAMPLE));
    type(form, "  interrupt for 'after'  ");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    expect([seen[0]?.instruction, seen[0]?.example?.text, seen[0]?.example?.route]).toEqual([
      "interrupt for 'after'",
      EXAMPLE.text,
      EXAMPLE.route,
    ]);
  });

  it("says the commit and clears the input on success", async () => {
    // Arrange
    const form = attached(forms().registry.formFor("turn-1", EXAMPLE));
    type(form, "interrupt for 'after'");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    const { status, input } = parts(form);
    expect([status.textContent, status.getAttribute("data-arm"), status.hidden, input.value]).toEqual([
      updatedSentence("0123456789abcdef"),
      "success",
      false,
      "",
    ]);
  });

  it("disables itself while the update is in flight", async () => {
    // Arrange: the answer is held until the test releases it.
    let release!: () => void;
    const held = new Promise<void>((resolve) => (release = resolve));
    const form = attached(
      forms(async () => {
        await held;
        return success();
      }).registry.formFor("turn-1", EXAMPLE),
    );
    type(form, "interrupt for 'after'");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    const { input, apply, dismiss, status } = parts(form);
    expect([input.disabled, apply.disabled, dismiss.disabled, status.getAttribute("data-arm")]).toEqual([
      true,
      true,
      true,
      "pending",
    ]);
    release();
    await settle();
  });

  it("is usable again once the update answers", async () => {
    // Arrange
    const form = attached(forms(refusal("unchanged")).registry.formFor("turn-1", EXAMPLE));
    type(form, "interrupt for 'after'");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    const { input, apply, dismiss } = parts(form);
    expect([input.disabled, apply.disabled, dismiss.disabled]).toEqual([false, false, false]);
  });

  it.each(oneofArms(UpdateClassifierPromptErrorSchema, "cause"))("says the %s refusal at the form", async (arm) => {
    // Arrange
    const form = attached(forms(refusal(arm)).registry.formFor("turn-1", EXAMPLE));
    type(form, "interrupt for 'after'");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    const { status } = parts(form);
    const say = UPDATE_CLASSIFIER_REFUSALS[arm];
    expect(say).toBeDefined();
    expect([status.getAttribute("data-arm"), status.textContent]).toEqual([
      arm,
      say((CAUSE_FILL[arm] ?? {}) as never),
    ]);
  });

  it("records a refusal at INFO through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const form = attached(forms(refusal("inProgress")).registry.formFor("turn-1", EXAMPLE));
    type(form, "interrupt for 'after'");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    const record = await forwardedRecord(capture, "tray.classifier-update.refused");
    expect(record.level.case).toBe("info");
  });

  it("files nothing on the warning chip while the card is on screen", async () => {
    // Arrange
    const { registry, reported } = forms(refusal("unchanged"));
    const form = attached(registry.formFor("turn-1", EXAMPLE));
    type(form, "interrupt for 'after'");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    expect(reported).toEqual([]);
  });

  it("files a refusal on the warning chip when the card left while it ran", async () => {
    // Arrange: the form is detached, as a delivered prompt's card is.
    const { registry, reported } = forms(refusal("unchanged"));
    const form = registry.formFor("turn-1", EXAMPLE);
    type(form, "interrupt for 'after'");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    expect(reported).toHaveLength(1);
  });

  it("says a transport failure at the form and records it at ERROR", async () => {
    // Arrange
    const capture = captureLogRecords();
    const form = attached(
      forms(() => {
        throw new ConnectError("daemon gone", Code.Internal);
      }).registry.formFor("turn-1", EXAMPLE),
    );
    type(form, "interrupt for 'after'");
    // Act
    parts(form).apply.click();
    await settle();
    // Assert
    const record = await forwardedRecord(capture, "tray.classifier-update.failed");
    expect([parts(form).status.getAttribute("data-arm"), parts(form).status.textContent, record.level.case]).toEqual([
      "failed",
      "daemon gone",
      "error",
    ]);
  });

  it("clears and hides on dismiss", () => {
    // Arrange
    const form = forms().registry.formFor("turn-1", EXAMPLE);
    form.toggle();
    type(form, "half a thought");
    // Act
    parts(form).dismiss.click();
    // Assert
    const { input, status, apply } = parts(form);
    expect([form.element.hidden, input.value, status.hidden, apply.disabled]).toEqual([true, "", true, true]);
  });
});

describe("updatedSentence", () => {
  it("names the commit by its short sha", () => {
    // Act / Assert
    expect(updatedSentence("0123456789abcdef")).toBe("classifier updated (commit 012345678)");
  });
});
