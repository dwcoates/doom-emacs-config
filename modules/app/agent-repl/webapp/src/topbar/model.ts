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
import { createControl, type Control } from "../control.js";
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
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { TopbarContext } from "./context.js";
import type { SentenceTable } from "../rpc/refuse.js";
import { drawNoSessionCell } from "./no-session.js";
import { sendPick } from "./pick.js";
import { asAnchor } from "./strip.js";

/**
 * The selector's hover (owner, 2026-10-01; design record 2026-10-02 decision
 * 2). Client-owned static copy.
 */
export const MODEL_TOOLTIP =
  "Changes the model this workspace uses for the rest of the session. Will cause token cache misses.";

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
    // NAMES NO MODEL, BY EITHER SPELLING. The synthetic marker and an empty
    // name are the two ways the daemon can serve the "let the CLI pick"
    // pseudo-row, and both are unselectable — a SetModel echoing one back would
    // be refused as not in the catalog. The marker check alone missed the empty
    // spelling, which drew as a nameless, pickable row.
    if (model.name !== marker && model.name.trim() !== "") return true;
    log.warn(`the daemon served a row that names no model (${JSON.stringify(model.name)}); skipping it`, {
      operation: "topbar.model-synthetic-option",
      context: { index },
    });
    return false;
  });
}

/**
 * What a cold refusal says, and where it points.
 *
 * `SetModelError.cold` is NOT a machinery failure: the shim raised the cold
 * gate rather than discarding a warm cache, and the remediation menu is the
 * GATE ROW's (pay | clear | compact), not this picker's. So the refusal names
 * the gate and the attention goes there — "the daemon could not be reached"
 * would send the reader looking for a fault that does not exist.
 */
export const COLD_REFUSAL_SENTENCE =
  "the context is cold: answer the cold gate, then set the model again";

/** The attribute the routed-to gate card is marked with. */
export const COLD_ATTENTION_ATTRIBUTE = "data-attention";

/** The value that attribute carries when the model picker routed here. */
export const COLD_ATTENTION_VALUE = "coldGate";

/** The footer notice's own hook, drawn only when no gate card is on the page. */
export const COLD_NOTICE_ATTRIBUTE = "data-footer-notice";

/**
 * The hook naming the model actually in force, carried by the control itself.
 *
 * THE NAME, NOT THE LABEL: two catalog rows may share a display name, so the
 * button's text cannot identify a selection and a reader — human or test —
 * asking "which model is this session on?" has nothing to read. The value is
 * `AgentModel.name`, the same echo token `[data-model-option]` carries on the
 * rows, so the selection and the offer are named in one vocabulary. Absent
 * when there is no selection: `[data-unselected]` is that state's hook, and a
 * name attribute holding an empty string would read as a nameless model.
 */
export const SELECTED_MODEL_ATTRIBUTE = "data-model";

/** The hook marking the offered row that is the current selection. */
export const SELECTED_OPTION_ATTRIBUTE = "data-selected";

/** The causes only SetModel can answer with. */
export const SET_MODEL_CAUSES = {
  noSession: () => "this workspace has no session to set a model on",
  notInCatalog: () => "that model is not in the served catalog",
  vendorRefused: (value: { detail: string }) => `the vendor refused: ${value.detail}`,
  cold: () => COLD_REFUSAL_SENTENCE,
} as unknown as SentenceTable;

/**
 * Send the reader to the cold gate.
 *
 * THE GATE ROW IS THE REMEDIATION, so the first choice is always the card
 * itself: it is marked so the eye lands on it. It is NOT scrolled to (owner
 * rule, 2026-09-23: the user owns the scroll, and a model pick is not one of
 * the feed's three implicit scroll causes). A page whose feed has not drawn
 * the gate (a history page scrolled elsewhere, a gate the daemon has not
 * pushed yet) gets a footer notice NAMING the gate instead, which is a
 * different fact and says so, rather than a silent no-op.
 */
export function routeToColdGate(doc: Document): "gate" | "notice" {
  const gate = doc.querySelector<HTMLElement>(`[data-unit="${COLD_ATTENTION_VALUE}"]`);
  if (gate !== null) {
    gate.setAttribute(COLD_ATTENTION_ATTRIBUTE, COLD_ATTENTION_VALUE);
    log.info("routed the reader to the cold gate row", {
      operation: "topbar.model-cold-gate",
      context: { routed_to: "gate" },
    });
    return "gate";
  }
  const footer = doc.querySelector<HTMLElement>('[data-component="footer"]');
  if (footer === null) {
    // The shell resolves the footer's mount by id at boot, so its absence here
    // is this page having no footer at all, not a missing notice.
    throw new Error("the page has neither a cold gate row nor a footer to notice on");
  }
  for (const stale of footer.querySelectorAll(`[${COLD_NOTICE_ATTRIBUTE}]`)) stale.remove();
  const notice = document.createElement("div");
  notice.className = "footer-notice";
  notice.setAttribute(COLD_NOTICE_ATTRIBUTE, COLD_ATTENTION_VALUE);
  notice.textContent = COLD_REFUSAL_SENTENCE;
  footer.append(notice);
  log.info("no cold gate row is drawn; noticed the gate on the footer", {
    operation: "topbar.model-cold-gate",
    context: { routed_to: "notice" },
  });
  return "notice";
}

/**
 * The selector: the button that names the selection and the reveal that offers
 * the alternatives.
 */
export function drawTopbarModelSelector(
  u: TopbarModelSelector | undefined,
  tc: TopbarContext,
): HTMLElement {
  // ABSENT IS "NO SESSION HAS STATED A MODEL", and the slot stays. See
  // `no-session.ts`: the strip has one shape, so a cell with no fact behind it
  // draws a dash rather than vanishing and shifting its neighbours.
  if (u === undefined) return drawNoSessionCell("model");
  log.debug("drawing the model selector", {
    operation: "topbar.model-selector",
    context: { options: u.options.length, selected: u.selected !== undefined },
  });

  const wrap = document.createElement("div");
  wrap.className = "topbar-model";
  wrap.title = MODEL_TOOLTIP;

  const button = createControl();
  button.className = "topbar-model-button";
  // PRESENCE, NOT AN EMPTY STRING: an unset selection is the placeholder, and
  // a served option whose display name happens to be empty is still a
  // selection — drawing both as the placeholder would erase the difference.
  button.textContent = u.selected?.displayName ?? MODEL_PLACEHOLDER;
  button.toggleAttribute("data-unselected", u.selected === undefined);
  wrap.append(button);

  // THE CONTROL CARRIES THE NAME, for the same reason the anchor and the click
  // do: `.topbar-model` is the element the DOM contract names (preamble §5b),
  // and the button is the label inside it. Omitted rather than emptied when
  // nothing is selected.
  if (u.selected !== undefined) {
    wrap.setAttribute(SELECTED_MODEL_ATTRIBUTE, selectedModelName(u.selected));
  }

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

/**
 * The selection's own echo token.
 *
 * Read through `requireMessage` like every other served submessage: a
 * selection with no model is a malformed push, and the guard that wraps the
 * draw is what turns it into a refusal instead of a silently unnamed chip.
 */
export function selectedModelName(selected: ModelOption): string {
  return requireMessage(selected.model, "TopbarModelSelector.selected.model").name;
}

/** The reveal: exactly the served options, in the served order. */
export function drawModelOptions(
  u: TopbarModelSelector,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: Control,
): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-model-options list-rows";
  const selected = u.selected === undefined ? undefined : selectedModelName(u.selected);
  for (const option of offerableOptions(u.options)) {
    const row = drawModelOption(option, tc, wrap, button);
    // The row that IS the selection, named by the same echo token the button
    // carries — so the reveal shows where the reader already is rather than
    // offering the current model as if it were a change.
    row.toggleAttribute(
      SELECTED_OPTION_ATTRIBUTE,
      selected !== undefined && row.getAttribute("data-model-option") === selected,
    );
    list.append(row);
  }
  return list;
}

/** One offered model. */
export function drawModelOption(
  option: ModelOption,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: Control,
): HTMLElement {
  const model = requireMessage(option.model, "ModelOption.model");
  const row = createControl();
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
  button: Control,
  row: Control,
): Promise<void> {
  const model = requireMessage(option.model, "ModelOption.model");
  log.info(`the reader picked the model ${model.name}`, {
    operation: "topbar.model-picked",
    context: { model: model.name },
  });
  await sendPick(
    {
      rpc: "SetModel",
      send: (client) => client.setModel({ workspace: tc.ctx.workspace, model }),
      schema: SetModelResponseSchema,
      causes: SET_MODEL_CAUSES,
      malformedOperation: "topbar.model-pick",
      unreadableOperation: "topbar.model-malformed-refusal",
      // The cold refusal is the only arm whose remediation lives on ANOTHER
      // surface, so it is the only one that moves the page.
      onRefused: (arm) => {
        if (arm === "cold") routeToColdGate(wrap.ownerDocument);
      },
    },
    tc,
    wrap,
    button,
    row,
  );
}
