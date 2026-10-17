/**
 * held-prompt — one prompt the daemon is holding, drawn as a parked card.
 *
 * A HELD PROMPT IS NOT A FEED ROW. It is daemon-owned pending intent the
 * vendor never saw, so it draws beside the conversation rather than in it. But
 * it IS the prompt it will become, so it is drawn by the one bubble
 * (src/bubble/draw.ts, owner rulings 2026-09-23): a prompt-role bubble on the
 * prompt rail whose fill is only 5% of the held grey-blue tint over the feed
 * (owner ruling, 2026-09-27), so it barely lifts off the page — the reader
 * sees their words have NOT reached the agent — at the one bubble width every
 * prompt takes (owner request, 2026-10-13), whose header strip is its status
 * BADGES, whose content is what the user said, painted by the one body
 * pipeline, and which collapses to ONE line ending in the one bubble's ellipsis when there is more, never a fade (owner
 * ruling, 2026-09-27), opened by the one toggle (expand.ts, armed on the held section).
 *
 * COLLAPSED IT IS ONE LINE AND ITS BADGES (owner spec, 2026-09-23; one line,
 * 2026-09-27). The rest —
 * the queued age, the classifier's rationale or failure detail, and the
 * actions — is the bubble's EXPAND-ONLY region (`expandOnly`,
 * src/bubble/draw.ts), shown once the same click that opens the text opens it.
 *
 * ONE BADGE, ALWAYS (owner ruling, 2026-10-08). The daemon ranks the standing
 * facts and sends the strongest as `badge`, naming the fact it stands for in
 * `stands_for`; every other fact is a `notes` sentence. `HELD_STATUS_BADGES` is
 * the ONE table from that fact to the badge's color class. THE WORDS ARE THE
 * DAEMON'S: the label is drawn VERBATIM, and its detail and the notes verbatim
 * in the expand-only details; this card composes no status sentence of its own
 * and never re-ranks the facts.
 *
 * WHY RELEASE IS NOT ALWAYS DRAWN. A release's MECHANISM is an interrupt.
 * Against a context cut or a session still coming up, the interrupt is
 * precisely what must not happen (the protos say so on each of those arms), so
 * the daemon refuses it. A button that acknowledges a
 * click and achieves nothing is worse than no button, so the card draws no
 * release there and says why in the actions row's title instead.
 *
 * EDIT BEGINS A DAEMON-OWNED EDIT (EditHeldPrompt). The card's Edit control
 * asks the daemon to claim the prompt; the editor's input takes its content
 * off the host view, and the card says it is being edited only because the
 * daemon's held item carries `editing`. A refused or failed begin is filed on
 * the topbar's warning chip (owner spec, 2026-09-23), never drawn at the row.
 *
 * FOLD ABOVE IS DAEMON-OFFERED (FoldHeldPrompt). The control is drawn only
 * while the entry carries `fold_above`, and it sends back that element's token
 * for the entry ahead, so the daemon folds into the entry the reader saw or
 * refuses; the refusal is said at the row, as Send now's and Cancel's are.
 *
 * WHY DROP RAISES A DOM EVENT. Dropping a held prompt discards text the user
 * typed and never got to send. Losing it silently is the exact failure the old
 * `heldPromptUnsentFailure` stub existed to prevent, so the text is handed
 * back on the way out: a bubbling `held-prompt-dropped` CustomEvent carrying
 * `{ text }`, which the dev-mode composer restores into an empty box. Nothing
 * listens in production (the composer is host-native) and the event is
 * harmless there — it is the seam, not a promise about who is on the far end.
 */
import { drawFeedUserPromptBody } from "../feed/rows/user-prompt.js";
import { labelledControl, whileControlsDisabled, type Control } from "../control.js";
import { getOption } from "@bufbuild/protobuf";
import type {
  HeldPrompt,
  HeldSessionAct,
  HeldPromptBadge,
  HeldPromptStatusNote,
  HeldPromptAwaitingDelivery,
  HeldPromptBuildRefreshHold,
  HeldPromptClassificationError,
  HeldPromptClassifying,
  HeldPromptColdGateAnsweredHold,
  HeldPromptColdGateHold,
  HeldPromptDaemonHeld,
  HeldPromptHoldForTurnEnd,
  HeldPromptAfterToolCall,
  HeldPromptInterject,
  HeldPromptMergeHold,
  HeldPromptQueuedAt,
  HeldPromptReconnectHold,
  HeldPromptShutdownHold,
  HeldPromptUninterruptibleTurn,
} from "../../../proto/gen/ts/frontend/v1/feed_held_pb";
import type {
  ImageBlock,
  UnsupportedBlock,
  TextBlock,
} from "../../../proto/gen/ts/conversation/v1/content_blocks_pb";
import {
  SessionCommand,
  SessionCommandSchema,
  session_command_spec,
} from "../../../proto/gen/ts/conversation/v1/slash_command_pb";
import type { TurnId } from "../../../proto/gen/ts/conversation/v1/turn_pb";
import type {
  UserContent,
  UserContentBlock,
  UserQuoteBlock,
  UserSaid,
} from "../../../proto/gen/ts/conversation/v1/user_pb";
import {
  UpdateHeldPromptResponseSchema,
  type UpdateHeldPromptError,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";
import {
  EditHeldPromptResponseSchema,
  type EditHeldPromptBeingEdited,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_edit_held_prompt_pb";
import {
  FoldHeldPromptResponseSchema,
  type FoldHeldPromptError,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_held_prompt_pb";
import { ConnectError } from "@connectrpc/connect";
import { controlPlaneFailed } from "../failure/sink.js";
import { formatTickedAge } from "../duration.js";
import { markdownSlot, unparsedProse } from "../bubble/body.js";
import { quoteSlot } from "../bubble/quote.js";
import { expandedOnly } from "../bubble/expanded-only.js";
import {
  BUBBLE_MORE_ELLIPSIS,
  ELLIPSIS_CAP_LINES,
  drawBubble,
} from "../bubble/draw.js";
import { log } from "../log.js";
import { MalformedView } from "../rpc/malformed.js";
import { callUnary } from "../rpc/unary.js";
import { isMalformedView } from "../rpc/malformed.js";
import { guardMalformed } from "../rpc/guard.js";
import {
  callFailure,
  clearRefusalsAfter,
  crossCuttingSentence,
  drawRefusalAfter,
  refusalOf,
  type SentenceTable,
} from "../rpc/refuse.js";
import {
  msOf,
  requireCase,
  requireMessage,
  unreachableArm,
} from "../rpc/strict.js";
import type { HeldContext } from "./context.js";
import {
  UPDATE_CLASSIFIER_LABEL,
  classifierRouteOf,
  type ClassifierUpdateForm,
} from "./classifier-update.js";

/** The class a held card's refusal notes carry. */
const REFUSAL_KIND = "queued-refusal";

/** The event a dropped prompt hands its text back on. */
export const DROPPED_EVENT = "held-prompt-dropped";

/** What `held-prompt-dropped` carries: the words the drop discarded. */
export interface HeldPromptDroppedDetail {
  text: string;
}

/** The release control's label: what the reader sees it do. */
export const SEND_NOW_LABEL = "Send now";

/** Why no release button is drawn, per the arm that forbids it. */
export const NO_RELEASE_TITLES: Readonly<Record<string, string>> = {
  uninterruptibleTurn:
    "a context cut is never interrupted for a queued prompt, so it cannot be sent ahead of it — it is delivered the moment that turn ends",
  reconnect:
    "the session is not up, so a prompt sent now would have nowhere to be delivered — it is delivered when the session reconnects",
  merge:
    "a merge is driving the session, so a prompt sent now would run inside it — it is delivered the moment the merge ends",
  coldGate:
    "the session is parked at its cold gate, and a prompt never answers the gate — the gate's answer decides it: clear discards it, pay and compact keep it for you to send, compact and resume sends it",
};

/**
 * One held prompt.
 *
 * The three held-section hooks ride the card itself: `data-held-turn` is the echoed
 * `TurnId`, `data-arm` the classification's arm, `data-hold` the hold's arm or
 * the literal `none` — absence stated rather than left to be inferred from a
 * missing attribute.
 */
export function drawHeldPrompt(
  u: HeldPrompt,
  tc: HeldContext,
  previous?: HTMLElement,
): HTMLElement {
  const path = "HeldPrompt";
  const turn = requireMessage(u.turn, `${path}.turn`);
  const said = requireMessage(u.said, `${path}.said`);
  const classification = requireCase(
    u.classification,
    `${path}.classification`,
  );
  const hold =
    u.hold.case === undefined ? null : requireCase(u.hold, `${path}.hold`);
  // `daemon_held` says a daemon condition holds the entry, and the hold arm
  // names it: the one without the other explains nothing.
  if (classification.case === "daemonHeld" && hold === null) {
    throw new MalformedView(
      `${path}.classification`,
      "daemon_held with no hold arm set",
    );
  }
  log.debug("drawing a held prompt", {
    operation: "held.held-prompt",
    context: {
      turn: turn.value,
      classification: classification.case,
      hold: hold === null ? "none" : hold.case,
      editing: u.editing !== undefined,
    },
  });

  // THE STRIP IS THE STATUS LINE (owner request, 2026-10-13): the one badge,
  // a quiet pill for every further standing fact, and the queued age. A
  // collapsed card shows everything but the rest of the words, so expanding
  // one only ever reveals more of what was said.
  const head = document.createElement("div");
  head.className = "queued-head";
  const verdict = drawClassification(classification, `${path}.classification`);
  if (hold !== null) drawHold(hold, `${path}.hold`);
  const drawn = drawHeldPromptBadge(
    requireMessage(u.badge, `${path}.badge`),
    u.notes,
    `${path}.badge`,
  );
  head.appendChild(drawn.pill);
  for (const note of drawn.notes) head.appendChild(note);
  head.appendChild(
    drawHeldPromptQueuedAt(
      requireMessage(u.queuedAt, `${path}.queued_at`),
      tc,
      `${path}.queued_at`,
    ),
  );
  if (hold !== null && hold.case === "shutdown" && drawn.status === "shutdown") {
    // The schedule id is a HOOK on the hold's badge, not words: it joins this
    // card to the shutdown it should explain.
    drawn.pill.setAttribute("data-schedule-id", hold.value.scheduleId);
  }
  // A HELD SESSION ACT IS DRAWN AS WHAT IT DOES; a prompt, as its feed row
  // will draw it: the daemon's resolved body, images and all.
  const content =
    u.act === undefined
      ? drawFeedUserPromptBody(requireMessage(u.body, `${path}.body`), `${path}.body`)
      : [drawHeldSessionAct(u.act, `${path}.act`)];

  // THE CONTROLS ARE ALWAYS SHOWN (owner request, 2026-10-13), in ONE footer
  // element, so a refusal an action draws beside its row lands in it too.
  const controls = document.createElement("div");
  controls.className = "queued-controls";
  // A CLASSIFIED PROMPT OFFERS "Update classifier": its form is this turn's
  // one element, kept across pushes (classifier-update.ts).
  const route = u.act === undefined ? classifierRouteOf(classification.case) : null;
  const classifierForm =
    route === null ? null : tc.classifierForms.formFor(turn.value, { text: spokenText(said), route });
  controls.appendChild(
    drawHeldPromptActions({
      tc,
      turn,
      classifierForm,
      text: spokenText(said),
      classification: classification.case,
      hold: hold === null ? null : hold.case,
      foldAbove:
        u.foldAbove === undefined
          ? null
          : requireMessage(u.foldAbove.above, `${path}.fold_above.above`),
    }),
  );

  // PREVIOUS, this turn's card from the section's last drawing, is updated in
  // place (drawBubble), so a push never replaces the box a reader opened.
  const card = drawBubble(
    {
      role: "prompt",
      variant: "held",
      hooks: holdCardHooks(hold === null ? null : hold.case),
      waving: false,
      strip: [head],
      // THE REASON ENDS THE FIRST LINE (owner request, 2026-10-13): floated
      // into the corner, so the collapsed line's ellipsis falls before it.
      ...(verdict.detail === null ? {} : { corner: verdict.detail }),
      content,
      footer: classifierForm === null ? [controls] : [controls, classifierForm.element],
      capLines: ELLIPSIS_CAP_LINES,
      more: BUBBLE_MORE_ELLIPSIS,
    },
    previous,
  ).bubble;
  card.setAttribute("data-held-turn", turn.value);
  card.setAttribute("data-arm", classification.case);
  card.setAttribute("data-hold", hold === null ? "none" : hold.case);
  if (u.editing === undefined) card.removeAttribute("data-editing");
  else card.setAttribute("data-editing", "true");
  if (u.coalesced === undefined) card.removeAttribute("data-coalesced");
  else card.setAttribute("data-coalesced", "true");
  if (u.act === undefined) card.removeAttribute("data-act");
  else
    card.setAttribute(
      "data-act",
      requireCase(u.act.act, `${path}.act.act`).case,
    );
  return card;
}

/**
 * A held session act: the change it makes when what is ahead of it ends,
 * "model → claude-opus-5-5" or "permission mode → plan".
 */
export function drawHeldSessionAct(
  u: HeldSessionAct,
  path: string,
): HTMLElement {
  const act = requireCase(u.act, `${path}.act`);
  const line = document.createElement("div");
  line.className = "queued-act";
  switch (act.case) {
    case "model":
      line.textContent = `model → ${act.value.model}`;
      break;
    case "permissionMode":
      line.textContent = `permission mode → ${act.value.mode}`;
      break;
    default: {
      const other: { case: string } = act;
      return unreachableArm(`${path}.act`, other.case);
    }
  }
  return line;
}

/**
 * The card's hooks: `held-right` is the RAIL every held prompt wears (owner
 * ruling 1, 2026-09-13) — the hook the held section is found by; the rail itself is the
 * prompt role's — and a hold that is not the turn's is named by its hook.
 * The hooks select NO border: a held prompt has none until it is received
 * (owner ruling, 2026-09-23); they stay as the stable names the integration
 * suite and the stylesheet's badge rules know a hold's kind by.
 */
function holdCardHooks(hold: string | null): string[] {
  switch (hold) {
    case null:
      return ["held-right"];
    case "shutdown":
    case "buildRefresh":
    case "merge":
      return ["held-right", "lease-card"];
    case "reconnect":
    case "coldGate":
    case "coldGateAnswered":
      // The card's `data-hold` names the wait; it wears no hook of its own.
      return ["held-right"];
    default:
      return unreachableArm("HeldPrompt.hold", hold);
  }
}

/** The ticking "queued 12s ago" age. The instant ships; the clock is ours. */
export function drawHeldPromptQueuedAt(
  u: HeldPromptQueuedAt,
  tc: HeldContext,
  path: string,
): HTMLElement {
  return drawQueuedAge(msOf(u.atMs, `${path}.at_ms`), tc, path);
}

/**
 * The ticking "queued 12s ago" age of a card queued at ATMS (epoch ms): every
 * held card's, whichever message carried the instant.
 */
export function drawQueuedAge(atMs: number, tc: HeldContext, path: string): HTMLElement {
  log.debug("drawing a held prompt's queued age", {
    operation: "held.held-prompt.queued-at",
    context: { path, at_ms: atMs },
  });
  const age = document.createElement("span");
  age.className = "queued-age";
  age.setAttribute("data-queued", "");
  const paint = (nowMs: number): void => {
    age.textContent = `queued ${formatTickedAge(nowMs - atMs)} ago`;
  };
  paint(tc.ctx.ticker.now());
  tc.onDispose(tc.ctx.ticker.subscribe(paint));
  return age;
}

/** What one classification arm contributes to the card. */
interface Verdict {
  /** The rationale or failure detail, when the arm carries one. */
  detail: HTMLElement | null;
}

/** Route the verdict. EVERY arm is named; an unknown one is a malformed view. */
function drawClassification(
  classification: NonNullable<HeldPrompt["classification"]> & { case: string },
  path: string,
): Verdict {
  switch (classification.case) {
    case "classifying":
      return drawHeldPromptClassifying(
        classification.value,
        `${path}.classifying`,
      );
    case "interject":
      return drawHeldPromptInterject(classification.value, `${path}.interject`);
    case "afterToolCall":
      return drawHeldPromptAfterToolCall(
        classification.value,
        `${path}.after_tool_call`,
      );
    case "holdForTurnEnd":
      return drawHeldPromptHoldForTurnEnd(
        classification.value,
        `${path}.hold_for_turn_end`,
      );
    case "uninterruptibleTurn":
      return drawHeldPromptUninterruptibleTurn(
        classification.value,
        `${path}.uninterruptible_turn`,
      );
    case "classificationError":
      return drawHeldPromptClassificationError(
        classification.value,
        `${path}.classification_error`,
      );
    case "daemonHeld":
      return drawHeldPromptDaemonHeld(
        classification.value,
        `${path}.daemon_held`,
      );
    case "awaitingDelivery":
      return drawHeldPromptAwaitingDelivery(
        classification.value,
        `${path}.awaiting_delivery`,
      );
    default: {
      const other: { case: string } = classification;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * Still deciding: a breathing in-flight badge and nothing else.
 *
 * No rationale, because none exists yet. The pulse is what says the state is
 * transient.
 */
export function drawHeldPromptClassifying(
  _u: HeldPromptClassifying,
  path: string,
): Verdict {
  log.debug("drawing a classifying held prompt", {
    operation: "held.held-prompt.classifying",
    context: { path },
  });
  return { detail: null };
}

/** Interjects: the green the owner named for a prompt that interrupts. */
export function drawHeldPromptInterject(
  u: HeldPromptInterject,
  path: string,
): Verdict {
  log.debug("drawing an interjecting held prompt", {
    operation: "held.held-prompt.interject",
    context: { path },
  });
  return { detail: rationale(u.rationale) };
}

/**
 * Joins the running turn after its current tool call: sent now with nothing
 * interrupted, so it wears the green of a prompt that goes to the agent now.
 */
export function drawHeldPromptAfterToolCall(
  u: HeldPromptAfterToolCall,
  path: string,
): Verdict {
  log.debug("drawing a held prompt joining the running turn", {
    operation: "held.held-prompt.after-tool-call",
    context: { path },
  });
  return { detail: rationale(u.rationale) };
}

/** Waits for the turn to end on its own, with the classifier's reason. */
export function drawHeldPromptHoldForTurnEnd(
  u: HeldPromptHoldForTurnEnd,
  path: string,
): Verdict {
  log.debug("drawing a hold-for-turn-end held prompt", {
    operation: "held.held-prompt.hold-for-turn-end",
    context: { path },
  });
  return { detail: rationale(u.rationale) };
}

/**
 * Behind a context cut. The badge's words name the cut and are the daemon's;
 * the command is still read here so an arm that names no command is refused as
 * a malformed view rather than drawn.
 */
export function drawHeldPromptUninterruptibleTurn(
  u: HeldPromptUninterruptibleTurn,
  path: string,
): Verdict {
  const literal = sessionCommandLiteral(u.command, `${path}.command`);
  log.debug("drawing an uninterruptible-turn held prompt", {
    operation: "held.held-prompt.uninterruptible-turn",
    context: { path, command: literal },
  });
  return { detail: null };
}

/** Nothing decided this: the error red, and the failure said out loud. */
export function drawHeldPromptClassificationError(
  u: HeldPromptClassificationError,
  path: string,
): Verdict {
  log.warn("drawing an unclassified held prompt", {
    operation: "held.held-prompt.classification-error",
    context: { path, detail: u.detail },
  });
  return { detail: reasonBadge(u.detail, `badge ${HELD_REASON_CLASS} queued-unclassified`) };
}

/**
 * No classifier ran and none will: a daemon condition holds the entry. It
 * draws NO badge — the hold arm's badge is what holds it.
 */
export function drawHeldPromptDaemonHeld(
  _u: HeldPromptDaemonHeld,
  path: string,
): Verdict {
  log.debug("drawing a daemon-held prompt", {
    operation: "held.held-prompt.daemon-held",
    context: { path },
  });
  return { detail: null };
}

/**
 * Nothing is classifying the entry and nothing holds it: it waits its place in
 * the queue. No rationale, because no verdict exists.
 */
export function drawHeldPromptAwaitingDelivery(
  _u: HeldPromptAwaitingDelivery,
  path: string,
): Verdict {
  log.debug("drawing a held prompt awaiting delivery", {
    operation: "held.held-prompt.awaiting-delivery",
    context: { path },
  });
  return { detail: null };
}

/**
 * The command a cut is running, spelled as the user types it.
 *
 * UNSPECIFIED carries no spec by design — it names no command — so it is a
 * malformed view here rather than a card explaining nothing.
 */
export function sessionCommandLiteral(
  command: SessionCommand,
  path: string,
): string {
  if (command === SessionCommand.UNSPECIFIED) {
    throw new MalformedView(path, "the session command is UNSPECIFIED");
  }
  // The enum's numeric value, named as a number: `v.number` is a plain number off the
  // descriptor, and comparing it to the enum type directly is the mismatch the linter flags.
  const wanted: number = command;
  const value = SessionCommandSchema.values.find((v) => v.number === wanted);
  if (value === undefined) {
    throw new MalformedView(path, `no session command has number ${command}`);
  }
  return getOption(value, session_command_spec).literal;
}

/** The hold's status, per arm. EVERY arm is named; an unknown one is malformed. */
function drawHold(
  hold: NonNullable<HeldPrompt["hold"]> & { case: string },
  path: string,
): HeldStatus {
  switch (hold.case) {
    case "shutdown":
      return drawHeldPromptShutdownHold(hold.value, `${path}.shutdown`);
    case "reconnect":
      return drawHeldPromptReconnectHold(
        hold.value,
        `${path}.reconnect`,
      );
    case "buildRefresh":
      return drawHeldPromptBuildRefreshHold(
        hold.value,
        `${path}.build_refresh`,
      );
    case "merge":
      return drawHeldPromptMergeHold(hold.value, `${path}.merge`);
    case "coldGate":
      return drawHeldPromptColdGateHold(hold.value, `${path}.cold_gate`);
    case "coldGateAnswered":
      return drawHeldPromptColdGateAnsweredHold(
        hold.value,
        `${path}.cold_gate_answered`,
      );
    default: {
      const other: { case: string } = hold;
      return unreachableArm(path, other.case);
    }
  }
}

/** Held for the scheduled restart. */
export function drawHeldPromptShutdownHold(
  u: HeldPromptShutdownHold,
  path: string,
): HeldStatus {
  log.debug("drawing a shutdown hold", {
    operation: "held.held-prompt.shutdown-hold",
    context: { path, schedule_id: u.scheduleId },
  });
  return "shutdown";
}

/** Held until the session reconnects. Empty on the wire: presence is the fact. */
export function drawHeldPromptReconnectHold(
  _u: HeldPromptReconnectHold,
  path: string,
): HeldStatus {
  log.debug("drawing a reconnect hold", {
    operation: "held.held-prompt.reconnect-hold",
    context: { path },
  });
  return "reconnect";
}

/** Held for the build refresh. Empty on the wire: presence is the fact. */
export function drawHeldPromptBuildRefreshHold(
  _u: HeldPromptBuildRefreshHold,
  path: string,
): HeldStatus {
  log.debug("drawing a build-refresh hold", {
    operation: "held.held-prompt.build-refresh-hold",
    context: { path },
  });
  return "buildRefresh";
}

/** Held until the merge ends. Empty on the wire: presence is the fact. */
export function drawHeldPromptMergeHold(
  _u: HeldPromptMergeHold,
  path: string,
): HeldStatus {
  log.debug("drawing a merge hold", {
    operation: "held.held-prompt.merge-hold",
    context: { path },
  });
  return "merge";
}

/** Held until the cold gate is answered. Empty on the wire: presence is the fact. */
export function drawHeldPromptColdGateHold(
  _u: HeldPromptColdGateHold,
  path: string,
): HeldStatus {
  log.debug("drawing a cold-gate hold", {
    operation: "held.held-prompt.cold-gate-hold",
    context: { path },
  });
  return "coldGate";
}

/**
 * Kept when the cold gate was answered with pay or compact: only the reader's
 * own "Send now" sends it. Empty on the wire: presence is the fact.
 */
export function drawHeldPromptColdGateAnsweredHold(
  _u: HeldPromptColdGateAnsweredHold,
  path: string,
): HeldStatus {
  log.debug("drawing a hold kept at the cold gate's answer", {
    operation: "held.held-prompt.cold-gate-answered-hold",
    context: { path },
  });
  return "coldGateAnswered";
}

/** What the actions row needs to know about the entry it acts on. */
interface ActionSpec {
  tc: HeldContext;
  turn: TurnId;
  text: string;
  classification: string;
  hold: string | null;
  /** The entry the "Fold Above" control folds into, or null when the daemon offers no fold. */
  foldAbove: TurnId | null;
  /** The "Update classifier" form, or null on a card no classifier decided. */
  classifierForm: ClassifierUpdateForm | null;
}

/** The two answers an entry takes, exactly as the request's arms name them. */
export type HeldAction = "release" | "drop";

/**
 * The entry's controls: release (labelled "Send now") and drop (labelled
 * "Cancel"), with Edit, the daemon-offered fold and Meta between them.
 *
 * IN-FLIGHT DISABLES THE ROW, not just the clicked button: the actions are
 * mutually exclusive answers about one entry, and a drop landing while a
 * release is in flight is a race the user should not be able to start.
 */
export function drawHeldPromptActions(spec: ActionSpec): HTMLElement {
  const forbids =
    spec.classification === "uninterruptibleTurn"
      ? spec.classification
      : spec.hold;
  const noReleaseTitle =
    forbids === null ? undefined : NO_RELEASE_TITLES[forbids];
  log.debug("drawing a held prompt's actions", {
    operation: "held.held-prompt.actions",
    context: {
      turn: spec.turn.value,
      release: noReleaseTitle === undefined,
      fold_above: spec.foldAbove?.value ?? null,
      update_classifier: spec.classifierForm !== null,
    },
  });

  const actions = document.createElement("div");
  actions.className = "queued-actions";

  // EVERY ENTRY OFFERS A RELEASE. Where an arm forbids the interrupt a release
  // needs, the daemon refuses it and the refusal is said at this control —
  // which is the contract's own answer to a verb that cannot run. Withholding
  // the button instead hid the reason in a hover and left the reader guessing
  // whether the held section had simply failed to draw it.
  // The release is LABELLED "Send now" (owner ruling, 2026-09-23): to the
  // reader it sends the held prompt at once. The wire verb is still `release`,
  // and so are the hooks it is found by.
  const release = actionButton("release", SEND_NOW_LABEL, spec);
  if (noReleaseTitle !== undefined) {
    release.title = noReleaseTitle;
    release.classList.add("queued-action-unlikely");
  }
  actions.appendChild(release);
  // EDIT follows Send now (owner spec, 2026-09-23).
  actions.appendChild(editButton(spec));
  // DAEMON-OFFERED: the fold stands exactly while the entry carries
  // `fold_above` (FoldHeldPrompt).
  if (spec.foldAbove !== null)
    actions.appendChild(foldAboveButton(spec, spec.foldAbove));
  if (spec.classifierForm !== null)
    actions.appendChild(updateClassifierButton(spec.classifierForm));
  // The drop is LABELLED "Cancel" (owner ruling, 2026-09-23): to the reader it
  // takes back a prompt they sent, and the text comes back to the composer. The
  // wire verb is still `drop`, and so are the hooks it is found by. It is
  // ALWAYS THE LAST control (owner request, 2026-10-13), the one destructive
  // action at the row's far end.
  actions.appendChild(actionButton("drop", "Cancel", spec));
  return actions;
}

/** One control, with its refusal drawn as this row's next sibling. */
function actionButton(
  action: HeldAction,
  label: string,
  spec: ActionSpec,
): Control {
  return labelledControl({
    className: `queued-action queued-action-${action}`,
    hook: ["data-held-action", action],
    label,
    onClick: (button) => {
      void guardMalformed(
        spec.tc.ctx,
        "held.held-prompt.action",
        run(action, spec, button),
      );
    },
  });
}

/** Issue the action; a refusal is said at the row that made the call. */
async function run(
  action: HeldAction,
  spec: ActionSpec,
  button: Control,
): Promise<void> {
  const succeeded = await rowCall(
    button,
    "UpdateHeldPrompt",
    action,
    spec.turn.value,
    async () => {
      const response = await callUnary(
        spec.tc.ctx,
        "UpdateHeldPrompt",
        (client) =>
          client.updateHeldPrompt({
            workspace: spec.tc.ctx.workspace,
            turn: spec.turn,
            action: heldAction(action),
          }),
        UpdateHeldPromptResponseSchema,
      );
      const result = requireCase(
        response.result,
        "UpdateHeldPromptResponse.result",
      );
      if (result.case === "success") return null;
      const cause = requireCase(
        result.value.cause,
        "UpdateHeldPromptError.cause",
      );
      const say =
        crossCuttingSentence("UpdateHeldPrompt", cause) ??
        updateHeldPromptRefusal(cause, action);
      return { arm: cause.case, say };
    },
  );
  // A drop DISCARDS TEXT THE USER TYPED. It leaves on the next push, so the
  // words are handed back here, on the way out, while they still exist.
  if (succeeded && action === "drop")
    handBackDroppedText(button, spec.text, spec.turn.value);
}

/** A refusal a row's call answered: the error's arm and the sentence said for it. */
interface RowRefusal {
  arm: string;
  say: string;
}

/**
 * THE ROW'S ONE CALL LIFECYCLE, shared by every held-prompt control whose
 * refusal is said at the row: the row's standing refusal is cleared and its
 * controls disabled for the call; a typed refusal (ATTEMPT answering one) or a
 * call that never landed is drawn as the row's next sibling and logged.
 * Answers whether the call succeeded.
 *
 * THE ROW IS DISABLED FOR EXACTLY THE CALL, whatever it answered. A success
 * does not promise a push: a release into a running turn registers an
 * interrupt and leaves the held view as it was until that turn ends, so a row
 * left disabled for the push to replace stayed dead for the whole turn
 * (2026-10-23). A card the push does take down takes its row with it.
 */
async function rowCall(
  button: Control,
  rpc: string,
  action: string,
  turn: string,
  attempt: () => Promise<RowRefusal | null>,
): Promise<boolean> {
  const row = button.parentElement;
  clearRefusalsAfter(row, REFUSAL_KIND);
  return whileControlsDisabled(row, async () => {
    try {
      const refused = await attempt();
      if (refused === null) {
        log.info(`${rpc} accepted a ${action}`, {
          operation: "held.held-prompt.action-accepted",
          context: { turn, action },
        });
        return true;
      }
      drawRefusalAfter(row, REFUSAL_KIND, refused.arm, refused.say);
      log.warn(`${rpc} refused a ${action}`, {
        operation: "held.held-prompt.action-refused",
        context: { turn, action, arm: refused.arm, sentence: refused.say },
      });
      return false;
    } catch (err) {
      // A MALFORMED VIEW IS NOT A TRANSPORT FAILURE: the daemon answered, and
      // this renderer could not read the answer. It travels up loudly rather
      // than being drawn as "could not be reached", which would be a lie.
      if (isMalformedView(err)) throw err;
      drawRefusalAfter(row, REFUSAL_KIND, "error", "the daemon could not be reached");
      log.error(`${rpc} failed for a ${action}: ${String(err)}`, {
        operation: "held.held-prompt.action-failed",
        context: { turn, action, cause: err },
      });
      return false;
    }
  });
}

/**
 * The "Update classifier" control: it reveals (or hides) this turn's form,
 * which says the change and sends it (classifier-update.ts).
 */
function updateClassifierButton(form: ClassifierUpdateForm): Control {
  return labelledControl({
    className: "queued-action queued-action-update-classifier",
    hook: ["data-held-action", "update-classifier"],
    label: UPDATE_CLASSIFIER_LABEL,
    onClick: () => form.toggle(),
  });
}

/** The fold control's label: what the reader sees it do. */
export const FOLD_ABOVE_LABEL = "Fold Above";

/**
 * The "Fold Above" control (FoldHeldPrompt): folds this prompt into the entry
 * directly ahead of it. Drawn only when the daemon's entry carries
 * `fold_above`, and it sends back exactly the token that element served, so
 * the daemon folds into the entry the reader saw above this one or refuses.
 */
function foldAboveButton(spec: ActionSpec, above: TurnId): Control {
  return labelledControl({
    className: "queued-action queued-action-fold",
    hook: ["data-held-action", "fold"],
    label: FOLD_ABOVE_LABEL,
    onClick: (button) => {
      void guardMalformed(
        spec.tc.ctx,
        "held.held-prompt.fold",
        fold(spec, above, button),
      );
    },
  });
}

/** Fold this prompt into ABOVE; a refusal is said at the row, as every row action's is. */
async function fold(
  spec: ActionSpec,
  above: TurnId,
  button: Control,
): Promise<void> {
  log.info("folding a held prompt into the one ahead", {
    operation: "held.held-prompt.fold",
    context: { turn: spec.turn.value, above: above.value },
  });
  await rowCall(button, "FoldHeldPrompt", "fold", spec.turn.value, async () => {
    const response = await callUnary(
      spec.tc.ctx,
      "FoldHeldPrompt",
      (client) =>
        client.foldHeldPrompt({
          workspace: spec.tc.ctx.workspace,
          turn: spec.turn,
          above,
        }),
      FoldHeldPromptResponseSchema,
    );
    const result = requireCase(
      response.result,
      "FoldHeldPromptResponse.result",
    );
    if (result.case === "success") return null;
    const cause = requireCase(result.value.cause, "FoldHeldPromptError.cause");
    const say =
      crossCuttingSentence("FoldHeldPrompt", cause) ??
      foldHeldPromptRefusal(cause);
    return { arm: cause.case, say };
  });
}

/** `FoldHeldPromptError`'s cause union, narrowed to a SET arm. */
type FoldHeldPromptCause = NonNullable<FoldHeldPromptError["cause"]> & {
  case: string;
};

/**
 * What each of FoldHeldPrompt's OWN arms says. Every one means the card is
 * stale or blocked, and nothing was folded; each says which.
 */
export function foldHeldPromptRefusal(cause: FoldHeldPromptCause): string {
  switch (cause.case) {
    case "noSuchHold":
      return "no prompt was ever held under this card";
    case "notHeld":
      return "this prompt is no longer held";
    case "notAPrompt":
      return "a queued session change cannot be folded";
    case "aboveMoved":
      return "the entry above has changed; nothing was folded";
    case "aboveNotAPrompt":
      return "the entry above is a session change, which takes no prompt";
    case "aboveOtherAuthor":
      return "the entry above has another author; folding would draw one author's words under the other's";
    case "beingEdited":
      return `a held prompt being edited cannot be folded (turn ${cause.value.editingTurn?.value ?? ""})`;
    case "beingDelivered":
      return BEING_DELIVERED;
    default: {
      const other: { case: string } = cause;
      return unreachableArm("FoldHeldPromptError.cause", other.case);
    }
  }
}

/**
 * What every held-prompt endpoint's `being_delivered` arm says: the prompt is
 * on its way to the agent, so it is neither held nor delivered yet, and a
 * failed delivery puts it back in the held section.
 */
export const BEING_DELIVERED =
  "this prompt is being delivered right now; if the delivery fails it returns to the tray";

/** What the warning chip names a failed or refused Edit as. */
export const EDIT_REQUEST = "edit a held prompt";

/**
 * What each of EditHeldPrompt's OWN arms says, when a begin is refused.
 * The cross-cutting four are worded once in `rpc/refusal.ts`.
 */
export const EDIT_REFUSALS: SentenceTable = {
  noSuchHold: () => "no prompt was ever held under this card",
  notHeld: () => "this prompt is no longer held",
  alreadyDelivered: () => "this prompt has already been delivered",
  beingEdited: (value: EditHeldPromptBeingEdited) =>
    `another held prompt is already being edited (turn ${value.editingTurn?.value ?? ""})`,
  notEditing: () => "no edit stands on this prompt",
  noEditor: () => "no editor is attached to edit this prompt in",
  beingDelivered: () => BEING_DELIVERED,
};

/** The Edit control: it begins a daemon-owned edit of this prompt. */
function editButton(spec: ActionSpec): Control {
  return labelledControl({
    className: "queued-action queued-action-edit",
    hook: ["data-held-action", "edit"],
    label: "Edit",
    onClick: (button) => {
      void guardMalformed(
        spec.tc.ctx,
        "held.held-prompt.edit",
        beginEdit(spec, button),
      );
    },
  });
}

/**
 * Ask the daemon to begin an edit. Success changes nothing here: the section's
 * push draws the editing badge, and the host view hands the editor the words.
 * A refusal or a failure is filed on the warning chip and logged.
 */
async function beginEdit(spec: ActionSpec, button: Control): Promise<void> {
  const row = button.parentElement;
  log.info("beginning an edit of a held prompt", {
    operation: "held.held-prompt.edit",
    context: { turn: spec.turn.value },
  });
  await whileControlsDisabled(row, async () => {
    let response;
    try {
      response = await callUnary(
        spec.tc.ctx,
        "EditHeldPrompt",
        (client) =>
          client.editHeldPrompt({
            workspace: spec.tc.ctx.workspace,
            turn: spec.turn,
            action: { case: "begin", value: {} },
          }),
        EditHeldPromptResponseSchema,
      );
    } catch (err) {
      if (!(err instanceof ConnectError)) throw err;
      const failed = callFailure(err);
      log.error(`EditHeldPrompt failed for a begin: ${failed.text}`, {
        operation: "held.held-prompt.edit-failed",
        context: { turn: spec.turn.value, arm: failed.arm },
      });
      spec.tc.ctx.failures.report(
        controlPlaneFailed(EDIT_REQUEST, failed.text),
      );
      return;
    }
    const result = requireCase(
      response.result,
      "EditHeldPromptResponse.result",
    );
    switch (result.case) {
      case "success":
        log.info("the daemon began the edit; its pushes draw it", {
          operation: "held.held-prompt.edit-begun",
          context: { turn: spec.turn.value },
        });
        return;
      case "error": {
        const said = refusalOf(
          result.value.cause,
          EDIT_REFUSALS,
          "EditHeldPromptError.cause",
        );
        log.info(`EditHeldPrompt refused a begin: ${said.text}`, {
          operation: "held.held-prompt.edit-refused",
          context: {
            turn: spec.turn.value,
            arm: said.arm,
            sentence: said.text,
          },
        });
        spec.tc.ctx.failures.report(
          controlPlaneFailed(EDIT_REQUEST, said.text),
        );
        return;
      }
      default: {
        const other: { case: string } = result;
        return unreachableArm("EditHeldPromptResponse.result", other.case);
      }
    }
  });
}

/** `UpdateHeldPromptError`'s cause union, narrowed to a SET arm. */
type UpdateHeldPromptCause = NonNullable<UpdateHeldPromptError["cause"]> & {
  case: string;
};

/**
 * What each of this endpoint's OWN arms says.
 *
 * The four cross-cutting causes are worded once in `rpc/refusal.ts`; these four
 * are about the hold itself, and three of them mean THE CARD IS ALREADY STALE
 * — the section's next push takes it down — so each says which kind of stale it
 * is rather than a single "refused" that would read as a dead button.
 */
export function updateHeldPromptRefusal(
  cause: UpdateHeldPromptCause,
  action: HeldAction,
): string {
  switch (cause.case) {
    case "noSuchHold":
      return "this prompt is no longer held";
    case "alreadyDelivered":
      return "this prompt has already been delivered";
    case "releaseRefused":
      return "the session would not take this prompt now";
    case "beingDelivered":
      return BEING_DELIVERED;
    default: {
      const other: { case: string } = cause;
      return unreachableArm(
        `UpdateHeldPromptError.cause (on a ${action})`,
        other.case,
      );
    }
  }
}

/**
 * The request's `action` arm, built by name.
 *
 * A switch rather than `{ case: action, value: {} }`: the two arms are two
 * distinct message types, and spelling them out is what makes a third action
 * a compile error here instead of a silently-typed object.
 */
function heldAction(
  action: HeldAction,
):
  | { case: "release"; value: Record<string, never> }
  | { case: "drop"; value: Record<string, never> } {
  switch (action) {
    case "release":
      return { case: "release", value: {} };
    case "drop":
      return { case: "drop", value: {} };
  }
}

/** Hand the dropped words back, bubbling, so a composer can restore them. */
function handBackDroppedText(
  button: HTMLElement,
  text: string,
  turn: string,
): void {
  log.info("handing a dropped prompt's text back", {
    operation: "held.held-prompt.dropped",
    context: { turn, length: text.length },
  });
  const detail: HeldPromptDroppedDetail = { text };
  button.dispatchEvent(
    new CustomEvent(DROPPED_EVENT, { detail, bubbles: true }),
  );
}

/**
 * Every fact a held card's one badge can stand for: `HeldPromptBadge.stands_for`'s
 * arms, by their generated case names.
 */
export type HeldStatus = NonNullable<HeldPromptBadge["standsFor"]["case"]>;

/** The semantic color a held badge takes: a class on the shared `.badge`. */
export type HeldBadgeTone = "ok" | "err" | "run" | "muted" | "amber" | "teal";

/**
 * THE ONE TABLE FROM A HELD STATUS TO ITS BADGE (owner spec, 2026-09-23). The
 * two anchors are the owner's: WAITING for the turn's end is red, INTERRUPTING
 * is green. The rest take the existing token that says the same thing:
 *
 *   - after this tool call: the prompt goes to the agent now, joining the
 *     running turn, so it is green like an interjection;
 *   - classifying: a model is running, the tool card's in-flight orange;
 *   - an uninterruptible turn: the prompt WAITS for the cut's end, so red;
 *   - a classification error: a failure, the error red;
 *   - a shutdown, build-refresh or merge hold: coordinated daemon work, amber;
 *   - a reconnect hold: the machinery bringing a session up, the
 *     hibernation teal;
 *   - a cold-gate hold: the session is not up either, waiting on the gate's
 *     answer, so the same teal;
 *   - awaiting delivery: nothing is acting on the entry, muted;
 *   - kept at the cold gate's answer: nothing acts on it until the reader
 *     sends it, so muted too;
 *   - editing (EditHeldPrompt): the prompt is being worked on in the editor,
 *     the in-flight orange.
 */
export const HELD_STATUS_BADGES = {
  classifying: "run",
  interject: "ok",
  afterToolCall: "ok",
  holdForTurnEnd: "err",
  uninterruptibleTurn: "err",
  classificationError: "err",
  shutdown: "amber",
  buildRefresh: "amber",
  merge: "amber",
  reconnect: "teal",
  coldGate: "teal",
  awaitingDelivery: "muted",
  coldGateAnswered: "muted",
  editing: "run",
} as const satisfies Record<HeldStatus, HeldBadgeTone>;

/** The class every held badge wears beside the shared `.badge`. */
export const HELD_BADGE_CLASS = "held-badge";

/**
 * The classes a badge for STATUS wears. A status the table does not name is a
 * malformed view, logged and thrown (`heldStatusTone`), never a badge in some
 * default color.
 */
export function heldBadgeClasses(status: string): string {
  return heldBadgeToneClasses(heldStatusTone(status));
}

/**
 * The color the table gives STATUS. A status the table does not name is a
 * malformed view, logged here and thrown.
 */
function heldStatusTone(status: string): HeldBadgeTone {
  if (!Object.hasOwn(HELD_STATUS_BADGES, status)) {
    log.error(`a held prompt status has no badge: ${status}`, {
      operation: "held.held-prompt.badge-unknown-status",
      context: { status },
    });
    throw new MalformedView(
      "HeldPrompt.status",
      `status '${status}' has no badge`,
    );
  }
  return HELD_STATUS_BADGES[status as HeldStatus];
}

/** The classes a held badge in TONE wears. */
function heldBadgeToneClasses(tone: HeldBadgeTone): string {
  return `badge ${HELD_BADGE_CLASS} ${tone}`;
}

/** The attribute a held card's badge names the fact it stands for by. */
export const HELD_STATUS_ATTRIBUTE = "data-held-status";

/**
 * A held card's one status badge: LABEL verbatim, in TONE's color, naming
 * STATUS, the fact it stands for. Every held card's badge is drawn here, a
 * held prompt's and an ingress prompt's (ingress-prompt.ts) alike.
 */
export function drawHeldBadgePill(label: string, tone: HeldBadgeTone, status: string): HTMLElement {
  const pill = document.createElement("span");
  pill.className = heldBadgeToneClasses(tone);
  pill.setAttribute(HELD_STATUS_ATTRIBUTE, status);
  pill.textContent = label;
  return pill;
}

/** The status badge: the daemon's label verbatim, and the table's color for STATUS. */
function badge(label: string, status: HeldStatus): HTMLElement {
  return drawHeldBadgePill(label, heldStatusTone(status), status);
}

/** The class a note on a fact the badge does not show wears, a quiet pill in the strip. */
export const HELD_NOTE_CLASS = "queued-status-note";

/** The class the reason badge ending the card's first line wears. */
export const HELD_REASON_CLASS = "held-reason";

/**
 * The wire's ONE badge, colored by the fact it stands for, its detail the
 * badge's hover title, and a quiet pill for every other standing fact.
 *
 * A badge with no fact or an empty label, and a note with an empty sentence,
 * are malformed views (feed_held.proto), logged and thrown, never drawn.
 */
export function drawHeldPromptBadge(
  wire: HeldPromptBadge,
  notes: readonly HeldPromptStatusNote[],
  path: string,
): { pill: HTMLElement; status: HeldStatus; notes: HTMLElement[] } {
  const status = requireCase(wire.standsFor, `${path}.stands_for`).case;
  if (wire.label === "") {
    log.error("a held prompt's badge carried an empty label", {
      operation: "held.held-prompt.badge-empty-label",
      context: { path, status },
    });
    throw new MalformedView(`${path}.label`, "the badge label is empty");
  }
  const pills: HTMLElement[] = [];
  for (const [index, n] of notes.entries()) {
    if (n.sentence === "") {
      log.error("a held prompt's note carried an empty sentence", {
        operation: "held.held-prompt.note-empty",
        context: { path: `${path}.notes[${index}]` },
      });
      throw new MalformedView(`HeldPrompt.notes[${index}].sentence`, "the note is empty");
    }
    const pill = document.createElement("span");
    pill.className = `badge ${HELD_NOTE_CLASS}`;
    pill.textContent = n.sentence;
    pills.push(pill);
  }
  const pill = badge(wire.label, status);
  if (wire.detail !== undefined) pill.title = wire.detail;
  return { pill, status, notes: pills };
}

/**
 * The classifier quoting itself, as the badge that ends the card's first line,
 * and only when it said something. Its full words are its hover title too,
 * since the collapsed badge may be cut short.
 */
function rationale(text: string): HTMLElement | null {
  if (text === "") return null;
  return reasonBadge(text, `badge ${HELD_REASON_CLASS}`);
}

/** The reason badge: TEXT in an element of CLASSNAME, titled with itself. */
function reasonBadge(text: string, className: string): HTMLElement {
  const reason = document.createElement("span");
  reason.className = className;
  reason.textContent = text;
  reason.title = text;
  return reason;
}

/**
 * What the user said, drawn from the record itself: the bubble's content,
 * block by block — a text block as a markdown slot the one body pipeline
 * paints, an image by reference, and nothing where the schema says a block
 * draws nothing. Only an editor's own ingress entry is drawn this way: no
 * daemon has accepted it, so none has parsed it, and its words are drawn as
 * the markdown they were typed as (`unparsedProse`). Everything a daemon
 * holds arrives drawn (frontend.v1.FeedUserPromptBody).
 */
export function drawUserSaid(u: UserSaid, path: string): HTMLElement[] {
  const content = requireMessage(u.content, `${path}.content`);
  log.debug("drawing what a user said", {
    operation: "held.held-prompt.said",
    context: { path, blocks: content.blocks.length },
  });
  const drawn: HTMLElement[] = [];
  for (const [index, block] of content.blocks.entries()) {
    const el = drawUserContentBlock(block, `${path}.content.blocks[${index}]`);
    if (el !== null) drawn.push(el);
  }
  return drawn;
}

/** One block, or nothing where the schema says a block draws nothing. */
export function drawUserContentBlock(
  u: UserContentBlock,
  path: string,
): HTMLElement | null {
  const block = requireCase(u.block, `${path}.block`);
  switch (block.case) {
    case "text":
      return drawTextBlock(block.value, `${path}.text`);
    case "image":
      return drawImageBlock(block.value, `${path}.image`);
    case "unsupported":
      return drawUnsupportedBlock(block.value, `${path}.unsupported`);
    case "quote":
      return drawQuoteBlock(block.value, `${path}.quote`);
    default: {
      const other: { case: string } = block;
      return unreachableArm(`${path}.block`, other.case);
    }
  }
}

/** Words, as markdown — a slot the one body pipeline paints, as a feed prompt's are. */
export function drawTextBlock(u: TextBlock, path: string): HTMLElement {
  log.debug("drawing a text block", {
    operation: "held.held-prompt.text-block",
    context: { path, length: u.text.length },
  });
  return markdownSlot("queued-text", unparsedProse(u.text));
}

/**
 * A reply's quote of an earlier bubble: drawn only once the card is expanded
 * (src/bubble/quote.ts), so the collapsed one line is the person's own words.
 */
export function drawQuoteBlock(u: UserQuoteBlock, path: string): HTMLElement {
  log.debug("drawing a quote block", {
    operation: "held.held-prompt.quote-block",
    context: { path, length: u.text.length },
  });
  return quoteSlot("queued-quote", unparsedProse(u.text));
}

/**
 * An image, BY REFERENCE.
 *
 * A URL is fetchable and draws as the picture. A host PATH is not — the daemon
 * runs on that host and the webview does not — so the card names the file
 * rather than drawing a broken image where the user's attachment should be.
 * Either is drawn only once the card is expanded (src/bubble/expanded-only.ts).
 */
export function drawImageBlock(u: ImageBlock, path: string): HTMLElement {
  const location = requireCase(u.location, `${path}.location`);
  log.debug("drawing an image block", {
    operation: "held.held-prompt.image-block",
    context: { path, location: location.case, media_type: u.mediaType },
  });
  switch (location.case) {
    case "url": {
      const image = document.createElement("img");
      image.className = "queued-image";
      expandedOnly(image);
      image.src = location.value.url;
      image.alt = u.mediaType;
      return image;
    }
    case "path": {
      const named = document.createElement("div");
      named.className = "queued-image-path";
      expandedOnly(named);
      named.setAttribute("data-host-path", location.value.path);
      named.textContent = location.value.path;
      return named;
    }
    default: {
      const other: { case: string } = location;
      return unreachableArm(`${path}.location`, other.case);
    }
  }
}

/**
 * A block this schema does not model.
 *
 * IT RENDERS AS NOTHING, exactly as the schema says — the arm exists so the
 * decision not to model the kind stays reversible from stored data, never so a
 * client can dig into the payload. The kind is logged so the gap is visible to
 * whoever would model it, and the card's own words are unaffected.
 */
export function drawUnsupportedBlock(u: UnsupportedBlock, path: string): null {
  log.warn(`a held prompt carried an unmodeled ${u.kind} block`, {
    operation: "held.held-prompt.unsupported-block",
    context: { path, kind: u.kind },
  });
  return null;
}

/** The words a drop hands back: the text blocks, joined as typed. */
function spokenText(said: UserSaid): string {
  const content: UserContent | undefined = said.content;
  if (content === undefined) return "";
  return content.blocks
    .filter((block) => block.block.case === "text")
    .map((block) => (block.block.value as TextBlock).text)
    .join("\n");
}
