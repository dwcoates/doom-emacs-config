/**
 * classifier-update — a classified held card's "Update classifier" control
 * (UpdateClassifierPrompt).
 *
 * A prompt the routing classifier held offers the reader a way to change HOW
 * the classifier decides. The control reveals an input; the reader says the
 * change in their own words, and the daemon has a headless model rewrite the
 * classifier's brief and commits it. The card's own words and verdict ride the
 * request as the worked example the rewrite is shown.
 *
 * THE FORM OUTLIVES THE PUSH. The tray redraws a card's expand-only details on
 * every push, and a push can land while the reader is typing (another prompt
 * queued, a badge changed). So the form is ONE element per held turn, kept by
 * `ClassifierUpdateForms` across pushes and handed back to every drawing of
 * that turn's card, where `drawBubble` keeps it in place: what the reader typed,
 * the focus, and an update in flight all survive a redraw. The registry lives
 * as long as the tray, and drops a turn's form once its card has left.
 *
 * AN OUTCOME IS SAID AT THE FORM, and a failure is ALSO filed on the warning
 * chip when the card has already left (its prompt was delivered while the
 * rewrite ran), so a refusal is never said to a form nobody can see.
 */
import { ConnectError } from "@connectrpc/connect";
import {
  ClassifierRoute,
  UpdateClassifierPromptResponseSchema,
  type UpdateClassifierPromptChangedDuringRewrite,
  type UpdateClassifierPromptCommitFailed,
  type UpdateClassifierPromptInProgress,
  type UpdateClassifierPromptRewriteFailed,
  type UpdateClassifierPromptRewriteRejected,
  type UpdateClassifierPromptUncommittedChanges,
  type UpdateClassifierPromptUnchanged,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_classifier_prompt_pb";
import type { HeldPrompt } from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import { labelledControl, type Control } from "../control.js";
import { controlPlaneFailed } from "../failure/sink.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { isMalformedView } from "../rpc/malformed.js";
import { callFailure, refusalOf, type SentenceTable } from "../rpc/refuse.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";

/** The control's label on the card's actions row. */
export const UPDATE_CLASSIFIER_LABEL = "Update classifier";

/** What the warning chip names a failed or refused update as. */
export const UPDATE_CLASSIFIER_REQUEST = "update the classifier";

/** The input's placeholder: what the reader is asked for. */
export const UPDATE_CLASSIFIER_PLACEHOLDER = "How should the classifier decide differently?";

/** The class the form wears, and the `data-says` prefix that keeps it in place. */
export const CLASSIFIER_FORM_CLASS = "classifier-update";

/** The held prompt a form's update is asked from, as the card shows it. */
export interface ClassifierExample {
  /** What the user said, the text blocks joined as typed. */
  text: string;
  /** The verdict the classifier gave it. */
  route: ClassifierRoute;
}

/**
 * The classifier's verdict a classification arm stands for, or null for an
 * arm no classifier decided — the only cards that offer the control are the
 * ones a classifier held.
 */
export function classifierRouteOf(
  classification: NonNullable<HeldPrompt["classification"]["case"]>,
): ClassifierRoute | null {
  switch (classification) {
    case "interject":
      return ClassifierRoute.INTERRUPT;
    case "afterToolCall":
      return ClassifierRoute.AFTER_TOOL_CALL;
    case "holdForTurnEnd":
      return ClassifierRoute.HOLD_FOR_TURN_END;
    case "classifying":
    case "uninterruptibleTurn":
    case "classificationError":
    case "daemonHeld":
      return null;
    default: {
      const other: string = classification;
      return unreachableArm("HeldPrompt.classification", other);
    }
  }
}

/**
 * What each of UpdateClassifierPrompt's arms says. Every arm leaves the
 * classifier's prompt exactly as it was, and each says why.
 */
export const UPDATE_CLASSIFIER_REFUSALS: SentenceTable = {
  inProgress: (_: UpdateClassifierPromptInProgress) => "another classifier update is already running",
  uncommittedChanges: (value: UpdateClassifierPromptUncommittedChanges) =>
    `${value.path} has uncommitted changes; commit or revert them first`,
  rewriteFailed: (value: UpdateClassifierPromptRewriteFailed) => `the rewrite did not answer: ${value.detail}`,
  rewriteRejected: (value: UpdateClassifierPromptRewriteRejected) =>
    `the rewrite was not a usable classifier prompt: ${value.detail}`,
  unchanged: (_: UpdateClassifierPromptUnchanged) => "the rewrite left the classifier prompt unchanged",
  changedDuringRewrite: (_: UpdateClassifierPromptChangedDuringRewrite) =>
    "the classifier prompt changed while it was being rewritten; nothing was written",
  commitFailed: (value: UpdateClassifierPromptCommitFailed) =>
    `git refused the commit, so the prompt was put back: ${value.detail}`,
};

/** The sentence a committed update is acknowledged with. */
export function updatedSentence(commit: string): string {
  return `classifier updated (commit ${commit.slice(0, 9)})`;
}

/** One held turn's form: its element, and the example it currently asks from. */
export interface ClassifierUpdateForm {
  /** The form, hidden until the control opens it. */
  readonly element: HTMLElement;
  /** Show the form and focus its input, or hide it when it is showing. */
  toggle(): void;
}

/** Every held turn's form, kept across the tray's pushes. */
export class ClassifierUpdateForms {
  private readonly forms = new Map<string, FormState>();

  constructor(private readonly ctx: AppContext) {}

  /** TURN's form, created on first ask, asking from EXAMPLE from now on. */
  formFor(turn: string, example: ClassifierExample): ClassifierUpdateForm {
    let state = this.forms.get(turn);
    if (state === undefined) {
      state = createForm(this.ctx, turn, example);
      this.forms.set(turn, state);
    }
    state.example = example;
    return state.form;
  }

  /** Drop the form of every turn not in HELD: its card has left the tray. */
  retain(held: readonly string[]): void {
    for (const turn of [...this.forms.keys()]) {
      if (!held.includes(turn)) this.forms.delete(turn);
    }
  }

  /** The turns that hold a form, for tests and logs. */
  turns(): string[] {
    return [...this.forms.keys()];
  }
}

/** A form and the example it asks from, which every push may restate. */
interface FormState {
  form: ClassifierUpdateForm;
  example: ClassifierExample;
}

/** Build one turn's form. */
function createForm(ctx: AppContext, turn: string, example: ClassifierExample): FormState {
  const element = document.createElement("div");
  element.className = CLASSIFIER_FORM_CLASS;
  // KEPT IN PLACE across pushes: drawBubble keeps chrome stating the same thing.
  element.setAttribute("data-says", `${CLASSIFIER_FORM_CLASS}:${turn}`);
  element.hidden = true;

  const input = document.createElement("textarea");
  input.className = "classifier-update-input";
  input.placeholder = UPDATE_CLASSIFIER_PLACEHOLDER;
  input.rows = 3;

  const row = document.createElement("div");
  row.className = "classifier-update-actions";
  const apply = button("apply", "Apply", () => {
    void guardMalformed(
      ctx,
      "tray.classifier-update.apply",
      submit(ctx, turn, state.example, { element, input, status, dismiss, syncApply }),
    );
  });
  const dismiss = button("dismiss", "Dismiss", () => {
    element.hidden = true;
    input.value = "";
    say(status, null, "");
    syncApply();
  });
  row.append(apply, dismiss);

  const status = document.createElement("div");
  status.className = "classifier-update-status";
  status.hidden = true;

  element.append(input, row, status);

  const state: FormState = {
    example,
    form: {
      element,
      toggle: () => {
        element.hidden = !element.hidden;
        log.debug("toggled a held card's classifier update form", {
          operation: "tray.classifier-update.toggle",
          context: { turn, open: !element.hidden },
        });
        if (!element.hidden) input.focus();
      },
    },
  };

  const syncApply = (): void => {
    apply.disabled = input.disabled || input.value.trim() === "";
  };
  input.addEventListener("input", syncApply);
  syncApply();

  return state;
}

/** The form's parts a submission drives. */
interface FormParts {
  element: HTMLElement;
  input: HTMLTextAreaElement;
  status: HTMLElement;
  dismiss: Control;
  syncApply(): void;
}

/** Send the update and say its outcome at the form. */
async function submit(ctx: AppContext, turn: string, example: ClassifierExample, parts: FormParts): Promise<void> {
  const instruction = parts.input.value.trim();
  log.info("asking the daemon to update the classifier", {
    operation: "tray.classifier-update.apply",
    context: { turn, route: ClassifierRoute[example.route], length: instruction.length },
  });
  setBusy(parts, true);
  say(parts.status, "pending", "updating the classifier…");
  try {
    const response = await callUnary(
      ctx,
      "UpdateClassifierPrompt",
      (client) =>
        client.updateClassifierPrompt({
          instruction,
          example: { text: example.text, route: example.route },
        }),
      UpdateClassifierPromptResponseSchema,
    );
    const result = requireCase(response.result, "UpdateClassifierPromptResponse.result");
    switch (result.case) {
      case "success":
        log.info("the daemon updated the classifier", {
          operation: "tray.classifier-update.updated",
          context: { turn, commit: result.value.commit, path: result.value.path },
        });
        parts.input.value = "";
        say(parts.status, "success", updatedSentence(result.value.commit));
        return;
      case "error": {
        const said = refusalOf(result.value.cause, UPDATE_CLASSIFIER_REFUSALS, "UpdateClassifierPromptError.cause");
        // A typed refusal is an ANSWER, recorded at INFO; the daemon records
        // its failures at ERROR where they happen.
        log.info(`UpdateClassifierPrompt refused: ${said.text}`, {
          operation: "tray.classifier-update.refused",
          context: { turn, arm: said.arm, sentence: said.text },
        });
        failed(ctx, parts, said.arm, said.text);
        return;
      }
      default: {
        const other: { case: string } = result;
        return unreachableArm("UpdateClassifierPromptResponse.result", other.case);
      }
    }
  } catch (err) {
    // A MALFORMED VIEW IS NOT A TRANSPORT FAILURE: the daemon answered and
    // this renderer could not read it, so it travels up loudly.
    if (isMalformedView(err) || !(err instanceof ConnectError)) throw err;
    const failure = callFailure(err);
    log.error(`UpdateClassifierPrompt failed: ${failure.text}`, {
      operation: "tray.classifier-update.failed",
      context: { turn, arm: failure.arm, cause: err },
    });
    failed(ctx, parts, failure.arm, failure.text);
  } finally {
    setBusy(parts, false);
  }
}

/** Say a refusal or failure at the form, and on the chip when the card has left. */
function failed(ctx: AppContext, parts: FormParts, arm: string, text: string): void {
  say(parts.status, arm, text);
  if (!parts.element.isConnected) ctx.failures.report(controlPlaneFailed(UPDATE_CLASSIFIER_REQUEST, text));
}

/** Disable the form while an update is in flight. */
function setBusy(parts: FormParts, busy: boolean): void {
  parts.input.disabled = busy;
  parts.dismiss.disabled = busy;
  parts.syncApply();
}

/** Show TEXT as the form's status under ARM, or hide the status for a null arm. */
function say(status: HTMLElement, arm: string | null, text: string): void {
  status.hidden = arm === null;
  if (arm === null) status.removeAttribute("data-arm");
  else status.setAttribute("data-arm", arm);
  status.textContent = text;
}

/** One form control. */
function button(action: string, label: string, onClick: () => void): Control {
  return labelledControl({
    className: `classifier-update-action classifier-update-${action}`,
    hook: ["data-classifier-action", action],
    label,
    onClick,
  });
}
