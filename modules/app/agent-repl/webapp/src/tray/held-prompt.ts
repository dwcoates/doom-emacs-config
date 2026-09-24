/**
 * held-prompt — one prompt the daemon is holding, drawn as a parked card.
 *
 * A HELD PROMPT IS NOT A FEED ROW. It is daemon-owned pending intent the
 * vendor never saw, so it draws beside the conversation rather than in it. But
 * it IS the prompt it will become, so it is drawn by the one bubble
 * (src/bubble/draw.ts, owner rulings 2026-09-23): a prompt-role bubble on the
 * prompt rail whose fill is the held grey with only a subtle blue hue — the
 * reader sees their words have NOT reached the agent — whose header strip is
 * its badges and queued age, whose content is what the user said, painted by
 * the one body pipeline, and which collapses at TWO lines behind the one
 * has-more fade, opened by the one toggle (expand.ts, armed on the tray).
 *
 * TWO INDEPENDENT AXES, TWO INDEPENDENT ARMS. `classification` says WHEN the
 * prompt runs relative to the turn in front of it; `hold` says WHAT ELSE is
 * holding it back, and is simply unset in the ordinary case. Both are drawn,
 * because a prompt held behind a scheduled bounce AND classified as an
 * interjection is telling the reader two different things and collapsing them
 * into one badge would lose one of them.
 *
 * WHY RELEASE IS NOT ALWAYS DRAWN. A release's MECHANISM is an interrupt.
 * Against a context cut, a keep-alive turn, or a session still coming up, the
 * interrupt is precisely what must not happen (the protos say so on each of
 * those three arms), so the daemon refuses it. A button that acknowledges a
 * click and achieves nothing is worse than no button, so the card draws no
 * release there and says why in the actions row's title instead.
 *
 * EDIT BEGINS A DAEMON-OWNED EDIT (EditHeldPrompt). The card's Edit control
 * asks the daemon to claim the prompt; the editor's input takes its content
 * off the host view, and the card says it is being edited only because the
 * daemon's tray entry carries `editing`. A refused or failed begin is filed on
 * the topbar's warning chip (owner spec, 2026-09-23), never drawn at the row.
 *
 * WHY DROP RAISES A DOM EVENT. Dropping a held prompt discards text the user
 * typed and never got to send. Losing it silently is the exact failure the old
 * `heldPromptUnsentFailure` stub existed to prevent, so the text is handed
 * back on the way out: a bubbling `held-prompt-dropped` CustomEvent carrying
 * `{ text }`, which the dev-mode composer restores into an empty box. Nothing
 * listens in production (the composer is host-native) and the event is
 * harmless there — it is the seam, not a promise about who is on the far end.
 */
import { getOption } from "@bufbuild/protobuf";
import type {
  HeldPrompt,
  HeldPromptBuildRefreshHold,
  HeldPromptClassificationError,
  HeldPromptClassifying,
  HeldPromptHoldForTurnEnd,
  HeldPromptInterject,
  HeldPromptKeepAliveHold,
  HeldPromptQueuedAt,
  HeldPromptSessionStartingHold,
  HeldPromptShutdownHold,
  HeldPromptUninterruptibleTurn,
} from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
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
import { ConnectError } from "@connectrpc/connect";
import { controlPlaneFailed } from "../failure/sink.js";
import { formatTickedAge } from "../duration.js";
import { markdownSlot } from "../bubble/body.js";
import { drawBubble } from "../bubble/draw.js";
import { log } from "../log.js";
import { MalformedView } from "../rpc/malformed.js";
import { callUnary } from "../rpc/unary.js";
import { isMalformedView } from "../rpc/malformed.js";
import { guardMalformed } from "../rpc/guard.js";
import { callFailure, crossCuttingSentence, refusalOf, type SentenceTable } from "../rpc/refuse.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { TrayContext } from "./context.js";

/** The event a dropped prompt hands its text back on. */
export const DROPPED_EVENT = "held-prompt-dropped";

/** What `held-prompt-dropped` carries: the words the drop discarded. */
export interface HeldPromptDroppedDetail {
  text: string;
}

/** Why no release button is drawn, per the arm that forbids it. */
export const NO_RELEASE_TITLES: Readonly<Record<string, string>> = {
  uninterruptibleTurn:
    "a context cut is never interrupted for a queued prompt, so there is nothing to release it past — it is delivered the moment that turn ends",
  keepAlive:
    "the keep-alive turn has to finish before this prompt can be submitted, so there is no order in which releasing it works",
  sessionStarting:
    "the session is not up yet, so a released prompt would have nowhere to be delivered",
};

/**
 * One held prompt.
 *
 * The three tray hooks ride the card itself: `data-held-turn` is the echoed
 * `TurnId`, `data-arm` the classification's arm, `data-hold` the hold's arm or
 * the literal `none` — absence stated rather than left to be inferred from a
 * missing attribute.
 */
export function drawHeldPrompt(u: HeldPrompt, tc: TrayContext, previous?: HTMLElement): HTMLElement {
  const path = "HeldPrompt";
  const turn = requireMessage(u.turn, `${path}.turn`);
  const said = requireMessage(u.said, `${path}.said`);
  const classification = requireCase(u.classification, `${path}.classification`);
  const hold = u.hold.case === undefined ? null : requireCase(u.hold, `${path}.hold`);
  log.debug("drawing a held prompt", {
    operation: "tray.held-prompt",
    context: {
      turn: turn.value,
      classification: classification.case,
      hold: hold === null ? "none" : hold.case,
      editing: u.editing !== undefined,
    },
  });

  const head = document.createElement("div");
  head.className = "queued-head";

  const verdict = drawClassification(classification, `${path}.classification`);
  head.appendChild(verdict.badge);
  // DAEMON-STATED: the badge stands exactly while the entry carries `editing`.
  if (u.editing !== undefined) head.appendChild(badge("editing", "queued-badge editing"));
  if (verdict.accepted !== null) head.appendChild(verdict.accepted);
  head.appendChild(
    drawHeldPromptQueuedAt(requireMessage(u.queuedAt, `${path}.queued_at`), tc, `${path}.queued_at`),
  );
  const content = drawUserSaid(said, `${path}.said`);

  const footer: HTMLElement[] = [];
  if (verdict.detail !== null) footer.push(verdict.detail);
  if (hold !== null) footer.push(drawHold(hold, `${path}.hold`));
  footer.push(
    drawHeldPromptActions({
      tc,
      turn,
      text: spokenText(said),
      classification: classification.case,
      hold: hold === null ? null : hold.case,
      accept: verdict.offersAccept,
    }),
  );

  // PREVIOUS, this turn's card from the tray's last drawing, is updated in
  // place (drawBubble), so a push never replaces the box a reader opened.
  const card = drawBubble(
    {
      role: "prompt",
      variant: "held",
      hooks: holdCardHooks(hold === null ? null : hold.case),
      working: false,
      strip: [head],
      content,
      footer,
      capLines: 2,
    },
    previous,
  ).bubble;
  card.setAttribute("data-held-turn", turn.value);
  card.setAttribute("data-arm", classification.case);
  card.setAttribute("data-hold", hold === null ? "none" : hold.case);
  if (u.editing === undefined) card.removeAttribute("data-editing");
  else card.setAttribute("data-editing", "true");
  // The acceptance is STATE OF THE CARD, not of a marker that only exists once
  // it is true: the arm that has an acceptance says which way it stands, and
  // the arms that have none say nothing at all.
  if (verdict.acceptedState === null) card.removeAttribute("data-accepted");
  else card.setAttribute("data-accepted", verdict.acceptedState ? "true" : "false");
  return card;
}

/**
 * The card's hooks: `held-right` is the RAIL every held prompt wears (owner
 * ruling 1, 2026-09-13) — the hook the tray is found by; the rail itself is the
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
      return ["held-right", "lease-card"];
    default:
      // A keep-alive and a session bring-up are the same statement — the
      // machinery holds this, no classifier judged it, it cannot be forced —
      // which is the frame the revival hold always wore.
      return ["held-right", "keep-alive-card"];
  }
}

/** The ticking "queued 12s ago" age. The instant ships; the clock is ours. */
export function drawHeldPromptQueuedAt(
  u: HeldPromptQueuedAt,
  tc: TrayContext,
  path: string,
): HTMLElement {
  const atMs = msOf(u.atMs, `${path}.at_ms`);
  log.debug("drawing a held prompt's queued age", {
    operation: "tray.held-prompt.queued-at",
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
  badge: HTMLElement;
  /** Whether this arm's acceptance stands, or null where it has none. */
  acceptedState: boolean | null;
  /** The confirmed marker, on an accepted hold. */
  accepted: HTMLElement | null;
  /** The rationale or failure detail, when the arm carries one. */
  detail: HTMLElement | null;
  /** Whether the accept button is this arm's to offer. */
  offersAccept: boolean;
}

/** Route the verdict. EVERY arm is named; an unknown one is a malformed view. */
function drawClassification(
  classification: NonNullable<HeldPrompt["classification"]> & { case: string },
  path: string,
): Verdict {
  switch (classification.case) {
    case "classifying":
      return drawHeldPromptClassifying(classification.value, `${path}.classifying`);
    case "interject":
      return drawHeldPromptInterject(classification.value, `${path}.interject`);
    case "holdForTurnEnd":
      return drawHeldPromptHoldForTurnEnd(classification.value, `${path}.hold_for_turn_end`);
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
    default: {
      const other: { case: string } = classification;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * Still deciding: a breathing badge and nothing else.
 *
 * No rationale, because none exists yet, and no accept, because there is no
 * verdict to confirm. The pulse is what says the state is transient.
 */
export function drawHeldPromptClassifying(_u: HeldPromptClassifying, path: string): Verdict {
  log.debug("drawing a classifying held prompt", {
    operation: "tray.held-prompt.classifying",
    context: { path },
  });
  return {
    badge: badge("queued — classifying", "queued-badge classifying"),
    acceptedState: null,
    accepted: null,
    detail: null,
    offersAccept: false,
  };
}

/** Interjects: the in-flight orange, because this verdict preempts the turn. */
export function drawHeldPromptInterject(u: HeldPromptInterject, path: string): Verdict {
  log.debug("drawing an interjecting held prompt", {
    operation: "tray.held-prompt.interject",
    context: { path },
  });
  return {
    badge: badge("interjects", "queued-badge interrupt"),
    acceptedState: null,
    accepted: null,
    detail: rationale(u.rationale),
    offersAccept: false,
  };
}

/**
 * Waits for the turn: the ONE arm with something to accept.
 *
 * Accepting changes nothing about delivery — it records that the user saw the
 * hold — so once `accepted` is true the button is gone and a quiet marker
 * stands in its place rather than a button that would say the same thing
 * twice.
 */
export function drawHeldPromptHoldForTurnEnd(u: HeldPromptHoldForTurnEnd, path: string): Verdict {
  const confirmed = u.accepted?.accepted === true;
  log.debug("drawing a hold-for-turn-end held prompt", {
    operation: "tray.held-prompt.hold-for-turn-end",
    context: { path, accepted: confirmed },
  });
  let accepted: HTMLElement | null = null;
  if (confirmed) {
    accepted = document.createElement("span");
    accepted.className = "queued-accepted";
    accepted.setAttribute("data-accepted", "true");
    accepted.textContent = "confirmed";
  }
  return {
    badge: badge("after this turn", "queued-badge"),
    acceptedState: confirmed,
    accepted,
    detail: rationale(u.rationale),
    offersAccept: !confirmed,
  };
}

/**
 * Behind a context cut: the badge NAMES the cut.
 *
 * The literal comes off the enum value's own `session_command_spec` option, so
 * the word on screen and the word the daemon recognizes are one definition
 * rather than two tables that agree until one is corrected.
 */
export function drawHeldPromptUninterruptibleTurn(
  u: HeldPromptUninterruptibleTurn,
  path: string,
): Verdict {
  const literal = sessionCommandLiteral(u.command, `${path}.command`);
  log.debug("drawing an uninterruptible-turn held prompt", {
    operation: "tray.held-prompt.uninterruptible-turn",
    context: { path, command: literal },
  });
  return {
    badge: uninterruptibleBadge(literal),
    acceptedState: null,
    accepted: null,
    detail: null,
    offersAccept: false,
  };
}

/** Nothing decided this: the warning tone, and the failure said out loud. */
export function drawHeldPromptClassificationError(
  u: HeldPromptClassificationError,
  path: string,
): Verdict {
  log.warn("drawing an unclassified held prompt", {
    operation: "tray.held-prompt.classification-error",
    context: { path, detail: u.detail },
  });
  const detail = document.createElement("div");
  detail.className = "queued-reason queued-unclassified";
  detail.textContent = u.detail;
  return {
    badge: badge("unclassified", "queued-badge unclassified"),
    acceptedState: null,
    accepted: null,
    detail,
    offersAccept: false,
  };
}

/**
 * The command a cut is running, spelled as the user types it.
 *
 * UNSPECIFIED carries no spec by design — it names no command — so it is a
 * malformed view here rather than a blank in the sentence.
 */
export function sessionCommandLiteral(command: SessionCommand, path: string): string {
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

/**
 * The badge that names the cut.
 *
 * The command is its OWN element (`[data-command]`), because it is the turn's
 * own token rather than a word in this sentence — the same hook a command row
 * carries in the feed.
 */
function uninterruptibleBadge(literal: string): HTMLElement {
  const element = badge("waits for ", "queued-badge uninterruptible");
  const command = document.createElement("code");
  command.className = "queued-command";
  command.setAttribute("data-command", "");
  command.textContent = literal;
  element.appendChild(command);
  element.appendChild(document.createTextNode(" to finish"));
  return element;
}

/** The hold's own standing explanation, per arm. */
function drawHold(
  hold: NonNullable<HeldPrompt["hold"]> & { case: string },
  path: string,
): HTMLElement {
  switch (hold.case) {
    case "shutdown":
      return drawHeldPromptShutdownHold(hold.value, `${path}.shutdown`);
    case "keepAlive":
      return drawHeldPromptKeepAliveHold(hold.value, `${path}.keep_alive`);
    case "sessionStarting":
      return drawHeldPromptSessionStartingHold(hold.value, `${path}.session_starting`);
    case "buildRefresh":
      return drawHeldPromptBuildRefreshHold(hold.value, `${path}.build_refresh`);
    default: {
      const other: { case: string } = hold;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * Held for the scheduled restart.
 *
 * The schedule id rides a `title` rather than the sentence: it joins the entry
 * to the shutdown it should explain, and it is a token, not prose.
 */
export function drawHeldPromptShutdownHold(u: HeldPromptShutdownHold, path: string): HTMLElement {
  log.debug("drawing a shutdown hold", {
    operation: "tray.held-prompt.shutdown-hold",
    context: { path, schedule_id: u.scheduleId },
  });
  // The schedule id is DRAWN, not only titled: it is the token that joins this
  // card to the shutdown it should explain, and a hover cannot be read back.
  const line = holdLine(`held for the scheduled restart (${u.scheduleId})`);
  line.setAttribute("data-schedule-id", u.scheduleId);
  return line;
}

/** Held behind a keep-alive turn; the turn it waits on rides a title. */
export function drawHeldPromptKeepAliveHold(u: HeldPromptKeepAliveHold, path: string): HTMLElement {
  const turn = requireMessage(u.turn, `${path}.turn`);
  log.debug("drawing a keep-alive hold", {
    operation: "tray.held-prompt.keep-alive-hold",
    context: { path, turn: turn.value },
  });
  const line = holdLine("held behind a keep-alive");
  line.title = `waiting on turn ${turn.value}`;
  return line;
}

/** Held until the session is up. Empty on the wire: presence is the fact. */
export function drawHeldPromptSessionStartingHold(
  _u: HeldPromptSessionStartingHold,
  path: string,
): HTMLElement {
  log.debug("drawing a session-starting hold", {
    operation: "tray.held-prompt.session-starting-hold",
    context: { path },
  });
  return holdLine("held until the session is up");
}

/** Held for the build refresh. Empty on the wire: presence is the fact. */
export function drawHeldPromptBuildRefreshHold(
  _u: HeldPromptBuildRefreshHold,
  path: string,
): HTMLElement {
  log.debug("drawing a build-refresh hold", {
    operation: "tray.held-prompt.build-refresh-hold",
    context: { path },
  });
  return holdLine("held for the build refresh");
}

/** What the actions row needs to know about the entry it acts on. */
interface ActionSpec {
  tc: TrayContext;
  turn: TurnId;
  text: string;
  classification: string;
  hold: string | null;
  accept: boolean;
}

/** The three answers an entry takes, exactly as the request's arms name them. */
export type HeldAction = "release" | "drop" | "accept";

/**
 * The entry's controls: release, drop (labelled "Cancel"), and — on one arm
 * only — accept.
 *
 * IN-FLIGHT DISABLES THE ROW, not just the clicked button: the three actions
 * are mutually exclusive answers about one entry, and a drop landing while a
 * release is in flight is a race the user should not be able to start.
 */
export function drawHeldPromptActions(spec: ActionSpec): HTMLElement {
  const forbids = spec.classification === "uninterruptibleTurn" ? spec.classification : spec.hold;
  const noReleaseTitle = forbids === null ? undefined : NO_RELEASE_TITLES[forbids];
  log.debug("drawing a held prompt's actions", {
    operation: "tray.held-prompt.actions",
    context: {
      turn: spec.turn.value,
      release: noReleaseTitle === undefined,
      accept: spec.accept,
    },
  });

  const actions = document.createElement("div");
  actions.className = "queued-actions";

  // EVERY ENTRY OFFERS A RELEASE. Where an arm forbids the interrupt a release
  // needs, the daemon refuses it and the refusal is said at this control —
  // which is the contract's own answer to a verb that cannot run. Withholding
  // the button instead hid the reason in a hover and left the reader guessing
  // whether the tray had simply failed to draw it.
  const release = actionButton("release", "Release", spec);
  if (noReleaseTitle !== undefined) {
    release.title = noReleaseTitle;
    release.classList.add("queued-action-unlikely");
  }
  actions.appendChild(release);
  // EDIT sits between Release and Cancel (owner spec, 2026-09-23).
  actions.appendChild(editButton(spec));
  // The drop is LABELLED "Cancel" (owner ruling, 2026-09-23): to the reader it
  // takes back a prompt they sent, and the text comes back to the composer. The
  // wire verb is still `drop`, and so are the hooks it is found by.
  actions.appendChild(actionButton("drop", "Cancel", spec));
  if (spec.accept) actions.appendChild(actionButton("accept", "Accept", spec));
  return actions;
}

/** One control, with its refusal drawn as this row's next sibling. */
function actionButton(action: HeldAction, label: string, spec: ActionSpec): HTMLButtonElement {
  const button = document.createElement("button");
  button.type = "button";
  button.className = `queued-action queued-action-${action}`;
  button.setAttribute("data-held-action", action);
  button.textContent = label;
  button.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    void guardMalformed(spec.tc.ctx, "tray.held-prompt.action", run(action, spec, button));
  });
  return button;
}

/** Issue the action; a refusal is said at the row that made the call. */
async function run(action: HeldAction, spec: ActionSpec, button: HTMLButtonElement): Promise<void> {
  const row = button.parentElement;
  clearRowRefusal(row);
  setRowDisabled(row, true);
  try {
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
    const result = requireCase(response.result, "UpdateHeldPromptResponse.result");
    if (result.case !== "success") {
      const cause = requireCase(
        (result.value).cause,
        "UpdateHeldPromptError.cause",
      );
      const say =
        crossCuttingSentence("UpdateHeldPrompt", cause) ?? updateHeldPromptRefusal(cause, action);
      drawRowRefusal(row, cause.case, say);
      log.warn(`UpdateHeldPrompt refused a ${action}`, {
        operation: "tray.held-prompt.action-refused",
        context: { turn: spec.turn.value, action, arm: cause.case, sentence: say },
      });
      return;
    }
    // A drop DISCARDS TEXT THE USER TYPED. It leaves on the next push, so the
    // words are handed back here, on the way out, while they still exist.
    if (action === "drop") handBackDroppedText(button, spec.text, spec.turn.value);
    // Success leaves the row alone: the tray's own push is what takes the card
    // down, and re-enabling a row about to be replaced would only flicker.
  } catch (err) {
    // A MALFORMED VIEW IS NOT A TRANSPORT FAILURE: the daemon answered, and
    // this renderer could not read the answer. It travels up loudly rather
    // than being drawn as "could not be reached", which would be a lie.
    if (isMalformedView(err)) throw err;
    drawRowRefusal(row, "error", "the daemon could not be reached");
    log.error(`UpdateHeldPrompt failed for a ${action}: ${String(err)}`, {
      operation: "tray.held-prompt.action-failed",
      context: { turn: spec.turn.value, action, cause: err },
    });
  } finally {
    // Only a refusal leaves the row on screen; re-enable so it can be retried.
    if (row !== null && row.parentElement?.querySelector(".queued-refusal") != null) {
      setRowDisabled(row, false);
    }
  }
}

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
};

/** The Edit control: it begins a daemon-owned edit of this prompt. */
function editButton(spec: ActionSpec): HTMLButtonElement {
  const button = document.createElement("button");
  button.type = "button";
  button.className = "queued-action queued-action-edit";
  button.setAttribute("data-held-action", "edit");
  button.textContent = "Edit";
  button.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    void guardMalformed(spec.tc.ctx, "tray.held-prompt.edit", beginEdit(spec, button));
  });
  return button;
}

/**
 * Ask the daemon to begin an edit. Success changes nothing here: the tray's
 * push draws the editing badge, and the host view hands the editor the words.
 * A refusal or a failure is filed on the warning chip and logged.
 */
async function beginEdit(spec: ActionSpec, button: HTMLButtonElement): Promise<void> {
  const row = button.parentElement;
  log.info("beginning an edit of a held prompt", {
    operation: "tray.held-prompt.edit",
    context: { turn: spec.turn.value },
  });
  setRowDisabled(row, true);
  try {
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
        operation: "tray.held-prompt.edit-failed",
        context: { turn: spec.turn.value, arm: failed.arm },
      });
      spec.tc.ctx.failures.report(controlPlaneFailed(EDIT_REQUEST, failed.text));
      return;
    }
    const result = requireCase(response.result, "EditHeldPromptResponse.result");
    switch (result.case) {
      case "success":
        log.info("the daemon began the edit; its pushes draw it", {
          operation: "tray.held-prompt.edit-begun",
          context: { turn: spec.turn.value },
        });
        return;
      case "error": {
        const said = refusalOf(result.value.cause, EDIT_REFUSALS, "EditHeldPromptError.cause");
        log.info(`EditHeldPrompt refused a begin: ${said.text}`, {
          operation: "tray.held-prompt.edit-refused",
          context: { turn: spec.turn.value, arm: said.arm, sentence: said.text },
        });
        spec.tc.ctx.failures.report(controlPlaneFailed(EDIT_REQUEST, said.text));
        return;
      }
      default: {
        const other: { case: string } = result;
        return unreachableArm("EditHeldPromptResponse.result", other.case);
      }
    }
  } finally {
    setRowDisabled(row, false);
  }
}

/** `UpdateHeldPromptError`'s cause union, narrowed to a SET arm. */
type UpdateHeldPromptCause = NonNullable<UpdateHeldPromptError["cause"]> & { case: string };

/**
 * What each of this endpoint's OWN arms says.
 *
 * The four cross-cutting causes are worded once in `rpc/refusal.ts`; these four
 * are about the hold itself, and three of them mean THE CARD IS ALREADY STALE
 * — the tray's next push takes it down — so each says which kind of stale it
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
    case "acceptNotApplicable":
      return "accept applies only to a prompt held for the turn's end";
    case "releaseRefused":
      return "the session would not take this prompt now";
    default: {
      const other: { case: string } = cause;
      return unreachableArm(`UpdateHeldPromptError.cause (on a ${action})`, other.case);
    }
  }
}

/**
 * The request's `action` arm, built by name.
 *
 * A switch rather than `{ case: action, value: {} }`: the three arms are three
 * distinct message types, and spelling them out is what makes a fourth action
 * a compile error here instead of a silently-typed object.
 */
function heldAction(
  action: HeldAction,
): { case: "release"; value: Record<string, never> }
  | { case: "drop"; value: Record<string, never> }
  | { case: "accept"; value: Record<string, never> } {
  switch (action) {
    case "release":
      return { case: "release", value: {} };
    case "drop":
      return { case: "drop", value: {} };
    case "accept":
      return { case: "accept", value: {} };
  }
}

/** Hand the dropped words back, bubbling, so a composer can restore them. */
function handBackDroppedText(button: HTMLElement, text: string, turn: string): void {
  log.info("handing a dropped prompt's text back", {
    operation: "tray.held-prompt.dropped",
    context: { turn, length: text.length },
  });
  const detail: HeldPromptDroppedDetail = { text };
  button.dispatchEvent(new CustomEvent(DROPPED_EVENT, { detail, bubbles: true }));
}

function setRowDisabled(row: Element | null, disabled: boolean): void {
  if (row === null) return;
  for (const control of row.querySelectorAll("button")) control.disabled = disabled;
}

function drawRowRefusal(row: Element | null, arm: string, message: string): void {
  if (row === null) return;
  const refusal = document.createElement("div");
  refusal.className = "refusal queued-refusal";
  refusal.setAttribute("data-arm", arm);
  refusal.textContent = message;
  row.after(refusal);
}

function clearRowRefusal(row: Element | null): void {
  row?.parentElement?.querySelectorAll(".queued-refusal").forEach((node) => node.remove());
}

/** The status pill. */
function badge(label: string, className: string): HTMLElement {
  const pill = document.createElement("span");
  pill.className = className;
  pill.textContent = label;
  return pill;
}

/** The classifier quoting itself: an aside, and only when it said something. */
function rationale(text: string): HTMLElement | null {
  if (text === "") return null;
  const reason = document.createElement("div");
  reason.className = "queued-reason";
  reason.textContent = text;
  return reason;
}

/** The app stating a rule, which is a different voice from the classifier's. */
function holdLine(text: string): HTMLElement {
  const line = document.createElement("div");
  line.className = "lease-reason";
  line.textContent = text;
  return line;
}

/**
 * What the user said: the bubble's content, block by block — a text block as a
 * markdown slot the one body pipeline paints, an image by reference, and
 * nothing where the schema says a block draws nothing.
 */
export function drawUserSaid(u: UserSaid, path: string): HTMLElement[] {
  const content = requireMessage(u.content, `${path}.content`);
  log.debug("drawing what a user said", {
    operation: "tray.held-prompt.said",
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
export function drawUserContentBlock(u: UserContentBlock, path: string): HTMLElement | null {
  const block = requireCase(u.block, `${path}.block`);
  switch (block.case) {
    case "text":
      return drawTextBlock(block.value, `${path}.text`);
    case "image":
      return drawImageBlock(block.value, `${path}.image`);
    case "unsupported":
      return drawUnsupportedBlock(block.value, `${path}.unsupported`);
    default: {
      const other: { case: string } = block;
      return unreachableArm(`${path}.block`, other.case);
    }
  }
}

/** Words, as markdown — a slot the one body pipeline paints, as a feed prompt's are. */
export function drawTextBlock(u: TextBlock, path: string): HTMLElement {
  log.debug("drawing a text block", {
    operation: "tray.held-prompt.text-block",
    context: { path, length: u.text.length },
  });
  return markdownSlot("queued-text", u.text);
}

/**
 * An image, BY REFERENCE.
 *
 * A URL is fetchable and draws as the picture. A host PATH is not — the daemon
 * runs on that host and the webview does not — so the card names the file
 * rather than drawing a broken image where the user's attachment should be.
 */
export function drawImageBlock(u: ImageBlock, path: string): HTMLElement {
  const location = requireCase(u.location, `${path}.location`);
  log.debug("drawing an image block", {
    operation: "tray.held-prompt.image-block",
    context: { path, location: location.case, media_type: u.mediaType },
  });
  switch (location.case) {
    case "url": {
      const image = document.createElement("img");
      image.className = "queued-image";
      image.src = location.value.url;
      image.alt = u.mediaType;
      return image;
    }
    case "path": {
      const named = document.createElement("div");
      named.className = "queued-image-path";
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
    operation: "tray.held-prompt.unsupported-block",
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
