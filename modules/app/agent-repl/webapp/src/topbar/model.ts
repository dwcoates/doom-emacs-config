/**
 * The model selector: what this session runs under, and what it may run under.
 *
 * THE SELECTION IS AN ECHO TOKEN. A pick sends back the option's own
 * `AgentModel` unchanged — the client never constructs a model id, never
 * normalizes one, and never sends the display text. The new selection then
 * arrives on the TOPBAR STREAM, not in the response: `SetModelSuccess` is
 * empty on purpose, so the button changes when the daemon says the session
 * changed and not a moment before.
 *
 * THE `<synthetic>` MARKER IS NOT A MODEL. The CLI answers it whenever it is
 * not running a real nameable model, and as an option it is a row that cannot
 * be selected — the daemon should never serve one, so an option carrying it is
 * logged at warn and SKIPPED rather than drawn. The literal is read by
 * REFLECTION off `conversation.v1.ModelMarker`'s own enum-value option, which
 * is the whole reason that extension exists: the spelling was re-declared six
 * times across three systems and kept aligned by review alone.
 *
 * CAPABILITIES RIDE THE OFFER because fast mode, auto mode and effort levels
 * are PER MODEL. They are drawn as small dim tags rather than controls: this
 * wave offers no way to set them, and a control that cannot be operated is
 * worse than a fact that can be read.
 */
import { getExtension } from "@bufbuild/protobuf";
import {
  AgentEffortLevel,
  AgentEffortLevelSchema,
  ModelMarker,
  ModelMarkerSchema,
  model_marker_literal,
  type ModelCapabilities,
  type ModelOption,
} from "../../../proto/gen/ts/conversation/v1/api_pb";
import { SetModelResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_model_pb";
import type { TopbarModelSelector } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { whileInFlight } from "../feed/cards/controls.js";
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { TopbarContext } from "./context.js";
import {
  drawTransportRefusal,
  drawTypedRefusal,
  drawUnreadableRefusal,
  type SentenceTable,
} from "../rpc/refuse.js";
import { asAnchor } from "./strip.js";

/** What the button says when the daemon reports no selection. */
export const MODEL_PLACEHOLDER = "model";

/**
 * The `<synthetic>` spelling, read off the schema rather than written here.
 *
 * Reflection, not a constant: the literal lives on the enum value's own option
 * so every runtime reads the same bytes back, which is what stops a corrected
 * spelling from leaving five stale copies behind.
 */
export function syntheticMarkerLiteral(): string {
  const value = ModelMarkerSchema.value[ModelMarker.SYNTHETIC];
  const options = value.proto.options;
  if (options === undefined) {
    // The schema stopped carrying the literal. Refusing to guess is the point:
    // a hard-coded "<synthetic>" here is the exact duplication the extension
    // was introduced to end.
    throw new Error("conversation.v1.ModelMarker.SYNTHETIC carries no model_marker_literal");
  }
  return getExtension(options, model_marker_literal);
}

/**
 * The options this build will DRAW: the served list minus anything carrying the
 * synthetic marker.
 *
 * Skipped, not refused: the daemon serving one is a producer defect the reader
 * cannot act on, and refusing the whole push over it would cost them the topbar
 * entirely.
 */
export function offerableOptions(options: readonly ModelOption[]): ModelOption[] {
  const marker = syntheticMarkerLiteral();
  return options.filter((option, index) => {
    const model = requireMessage(option.model, `TopbarModelSelector.options[${index}].model`);
    if (model.name !== marker) return true;
    log("warn", `the daemon served the ${marker} marker as a selectable model; skipping it`, {
      operation: "topbar.model-synthetic-option",
      context: { index },
    });
    return false;
  });
}

/** The causes only SetModel can answer with. */
export const SET_MODEL_CAUSES = {
  noSession: () => "this workspace has no session to set a model on",
  notInCatalog: () => "that model is not in the served catalog",
  vendorRefused: (value: { detail: string }) => `the vendor refused: ${value.detail}`,
} as unknown as SentenceTable;

/**
 * The selector: the button that names the selection and the reveal that offers
 * the alternatives.
 */
export function drawTopbarModelSelector(u: TopbarModelSelector, tc: TopbarContext): HTMLElement {
  log("debug", "drawing the model selector", {
    operation: "topbar.model-selector",
    context: { options: u.options.length, selected: u.selected !== undefined },
  });

  const wrap = document.createElement("div");
  wrap.className = "topbar-model";

  const button = document.createElement("button");
  button.type = "button";
  button.className = "topbar-model-button";
  // PRESENCE, NOT AN EMPTY STRING: an unset selection is the placeholder, and
  // a served option whose display name happens to be empty is still a
  // selection — drawing both as the placeholder would erase the difference.
  button.textContent = u.selected?.displayName ?? MODEL_PLACEHOLDER;
  button.toggleAttribute("data-unselected", u.selected === undefined);
  wrap.append(button);

  // THE CONTROL IS THE WRAP, not the label inside it. `.topbar-model` is the
  // hook the DOM contract names (preamble §5b), so the anchor and the click
  // both live on it: a reader clicking anywhere in the chip — the label or the
  // padding beside it — opens the same reveal, and the layer's outside-click
  // handler spares the whole control rather than one node of it.
  asAnchor(wrap, "model");
  const body = (): HTMLElement => drawModelOptions(u, tc, wrap, button);
  tc.reveals.register("model", "model", body);
  wrap.addEventListener("click", () => {
    tc.reveals.toggle("model", "model", body);
  });
  return wrap;
}

/** The reveal: exactly the served options, in the served order. */
export function drawModelOptions(
  u: TopbarModelSelector,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: HTMLButtonElement,
): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-model-options list-rows";
  for (const option of offerableOptions(u.options)) {
    list.append(drawModelOption(option, tc, wrap, button));
  }
  return list;
}

/** One offered model. */
export function drawModelOption(
  option: ModelOption,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: HTMLButtonElement,
): HTMLElement {
  const model = requireMessage(option.model, "ModelOption.model");
  const row = document.createElement("button");
  row.type = "button";
  row.className = "topbar-model-option";
  row.setAttribute("data-model-option", model.name);

  const name = document.createElement("span");
  name.className = "topbar-model-name";
  name.textContent = option.displayName;
  row.append(name);

  // EMPTY IS ABSENT (the proto says so outright): an empty description row
  // would read as a description that failed to load.
  if (option.description !== "") {
    const description = document.createElement("span");
    description.className = "topbar-model-description";
    description.textContent = option.description;
    row.append(description);
  }

  if (option.capabilities !== undefined) {
    row.append(drawModelCapabilities(option.capabilities));
  }

  row.addEventListener("click", () => {
    void pickModel(option, tc, wrap, button, row);
  });
  return row;
}

/** The capability tags: what this model can do, as the vendor declares it. */
export function drawModelCapabilities(u: ModelCapabilities): HTMLElement {
  const tags = document.createElement("span");
  tags.className = "topbar-model-tags";
  if (u.supportsFastMode) tags.append(tag("fast"));
  if (u.supportsAutoMode) tags.append(tag("auto"));
  if (u.supportsAdaptiveThinking) tags.append(tag("adaptive"));
  const effort = requireCase(u.effortSupport, "ModelCapabilities.effort_support");
  switch (effort.case) {
    case "effortUnsupported":
      // Nothing drawn. THE ARM IS THE ANSWER, and "this model takes no effort
      // level" is not a capability to advertise beside the ones it has.
      break;
    case "effortSupported":
      for (const level of effort.value.levels) tags.append(tag(effortLevelName(level)));
      break;
    default: {
      const other: { case: string } = effort;
      return unreachableArm("ModelCapabilities.effort_support", other.case);
    }
  }
  return tags;
}

function tag(text: string): HTMLElement {
  const element = document.createElement("span");
  element.className = "topbar-model-tag";
  element.textContent = text;
  return element;
}

/**
 * An effort level's word, read off the generated enum's own value names.
 *
 * The enum is a closed vocabulary shared with the response record, so the name
 * comes from the schema rather than from a table here that could drift from it.
 */
export function effortLevelName(level: AgentEffortLevel): string {
  const value = AgentEffortLevelSchema.value[level];
  if (value === undefined || level === AgentEffortLevel.UNSPECIFIED) {
    return unreachableArm("AgentEffortLevel", String(level));
  }
  return value.localName.toLowerCase();
}

/**
 * Send the pick, with the option row and the button inert while it is
 * unanswered.
 *
 * The refusal draws at the SELECTOR rather than inside the reveal, because the
 * reveal closes on the next click and a refusal the reader never sees is a
 * click that silently did nothing.
 */
export async function pickModel(
  option: ModelOption,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: HTMLButtonElement,
  row: HTMLButtonElement,
): Promise<void> {
  const model = requireMessage(option.model, "ModelOption.model");
  log("info", `the reader picked the model ${model.name}`, {
    operation: "topbar.model-picked",
    context: { model: model.name },
  });
  const answered = await whileInFlight([row, button], () =>
    callUnary(
      tc.ctx,
      "SetModel",
      (client) => client.setModel({ workspace: tc.ctx.workspace, model }),
      SetModelResponseSchema,
    ),
  );
  if ("failed" in answered) {
    drawTransportRefusal(wrap);
    return;
  }
  try {
    const result = requireCase(answered.value.result, "SetModelResponse.result");
    switch (result.case) {
      case "success":
        // Nothing is drawn: the new selection arrives on the topbar stream. The
        // reveal closes, because the reader's question has been answered.
        tc.reveals.close();
        return;
      case "error":
        drawTypedRefusal(wrap, "SetModelError.cause", "SetModel", result.value.cause, SET_MODEL_CAUSES);
        button.disabled = false;
        return;
      default: {
        const other: { case: string } = result;
        return unreachableArm("SetModelResponse.result", other.case);
      }
    }
  } catch (err) {
    button.disabled = false;
    if (!drawUnreadableRefusal(tc.ctx, wrap, "topbar.model-malformed-refusal", err)) throw err;
  }
}
