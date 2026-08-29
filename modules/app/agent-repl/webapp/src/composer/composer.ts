/**
 * composer — the box a prompt is typed into, and the one place SubmitPrompt is
 * called from.
 *
 * WHERE IT EXISTS. Production runs COMPOSER-LESS: the root composer is
 * host-native (Emacs), and the webview only ever draws one in dev mode
 * (`&composer=1`) or inside a subagent bubble, where the prompt is addressed to
 * THAT agent through `SubmitPromptRequest.feed`. Asked to mount anywhere else,
 * this draws nothing at all rather than a disabled box the user would wonder
 * about.
 *
 * THE GATE IS THE PRIMARY DEFENSE, THE REFUSAL IS THE RACE FALLBACK. A merging,
 * closing or disconnected workspace CLOSES the composer through the gate, which
 * is pushed state the wiring resolves from the footer. `SubmitPromptError` arms
 * exist for the submission already in flight when the state flipped: they are
 * drawn INLINE at the composer, per typed arm, and they are the submitter's own
 * — no pushed view carries them.
 *
 * TEXT IS NEVER LOST. That is the whole discipline of this module and the honest
 * home of the old `heldPromptUnsentFailure` stub: a refusal keeps the words in
 * the box, a transport failure keeps them, an unreadable answer keeps them, and
 * a prompt the user DROPS from the hold tray is offered back through the
 * `held-prompt-dropped` event when the box is empty. The idempotency key is
 * minted once per attempt and REUSED while the same unsent text stands, so
 * pressing send again after a failure is the same submission rather than a
 * second turn.
 */
import { create } from "@bufbuild/protobuf";
import { PromptOrigin } from "../../../proto/gen/ts/conversation/v1/prompt_origin_pb";
import type { TurnId } from "../../../proto/gen/ts/conversation/v1/turn_pb";
import { UserSaidSchema } from "../../../proto/gen/ts/conversation/v1/user_pb";
import type { UserSaid } from "../../../proto/gen/ts/conversation/v1/user_pb";
import type { SubmitPromptCommandPanel } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import {
  SubmitPromptRequestSchema,
  SubmitPromptResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import type { SubmitPromptRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { DROPPED_EVENT, type HeldPromptDroppedDetail } from "../tray/held-prompt.js";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

/** Whether the composer accepts input at all. */
export type ComposerGateState = "open" | "closed";

/**
 * The gate, owned by whoever watches the footer and read by every composer.
 *
 * The REASON rides with the state because the notice the user reads ("merge in
 * flight", "the workspace is closing") is the footer's word, not this module's:
 * a table here mapping status arms to sentences would be exactly the
 * client-side derivation the architecture forbids.
 */
export interface ComposerGate {
  current(): ComposerGateState;
  /** Set the gate; REASON is the sentence a closed composer shows. */
  set(state: ComposerGateState, reason?: string): void;
  subscribe(fn: (state: ComposerGateState, reason?: string) => void): () => void;
}

/** A gate that starts open and remembers its latest reason. */
export function createComposerGate(): ComposerGate {
  let state: ComposerGateState = "open";
  let reason: string | undefined;
  const listeners = new Set<(state: ComposerGateState, reason?: string) => void>();
  return {
    current: () => state,
    set(next: ComposerGateState, nextReason?: string): void {
      state = next;
      reason = nextReason;
      log("debug", `the composer gate is ${next}`, {
        operation: "composer.gate",
        context: { state: next, reason: nextReason },
      });
      for (const fn of [...listeners]) fn(state, reason);
    },
    subscribe(fn: (state: ComposerGateState, reason?: string) => void): () => void {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
  };
}

/** What a mounted composer answers with. */
export interface ComposerHandle extends Handle {
  /** The TurnId of the last accepted prompt, for matching a feed row to it. */
  lastTurn(): TurnId | undefined;
}

export interface ComposerOptions {
  /** The bubble's feed this composer addresses; unset means the root feed. */
  feed?: FeedId;
  gate: ComposerGate;
  /** Where a recognized command's resolved panel is handed for drawing. */
  onPanel: (panel: SubmitPromptCommandPanel) => void;
}

/** How a composer factory is called for a bubble's own box. */
export type ComposerFactory = (host: HTMLElement, feed: FeedId) => Handle;

/**
 * Mount a composer on HOST.
 *
 * A composer with NEITHER `composerEnabled` NOR a `feed` has no business
 * existing (that is production's root composer, which is Emacs's), so it draws
 * nothing and answers an inert handle.
 */
export function mountComposer(
  host: HTMLElement,
  ctx: AppContext,
  opts: ComposerOptions,
): ComposerHandle {
  if (!ctx.composerEnabled && opts.feed === undefined) {
    log("info", "not mounting a root composer: the host owns it", {
      operation: "composer.mount-skipped",
    });
    return { dispose: () => undefined, lastTurn: () => undefined };
  }
  log("debug", "mounting a composer", {
    operation: "composer.mount",
    context: { feed: opts.feed?.value ?? "root" },
  });

  const root = document.createElement("div");
  root.className = "composer-box";

  const notice = document.createElement("div");
  notice.className = "composer-notice";
  root.appendChild(notice);

  const input = document.createElement("textarea");
  input.className = "composer-input";
  input.rows = 2;
  root.appendChild(input);

  const send = document.createElement("button");
  send.type = "button";
  send.className = "composer-send";
  send.setAttribute("data-composer-send", "");
  send.textContent = "Send";
  root.appendChild(send);
  host.appendChild(root);

  let lastTurn: TurnId | undefined;
  /** The key of the attempt currently standing, and the text it was for. */
  let pending: { key: string; text: string } | null = null;
  /** One submission at a time; a second press while one is in flight is noise. */
  let inFlight = false;

  const applyGate = (state: ComposerGateState, reason?: string): void => {
    const closed = state === "closed";
    // The TEXTAREA stays enabled on purpose: the draft remains editable while
    // the gate is shut, exactly as the legacy merge gate left it.
    send.disabled = closed;
    notice.textContent = closed ? (reason ?? "composer closed") : "";
    root.toggleAttribute("data-gate-closed", closed);
  };
  applyGate(opts.gate.current());
  const unsubscribeGate = opts.gate.subscribe(applyGate);

  const submit = (): void => {
    if (send.disabled || inFlight) return;
    const text = input.value.trim();
    if (text === "") return;
    // THE KEY IS THE ATTEMPT'S, NOT THE PRESS'S: the same unsent words retried
    // are the same submission, and the daemon refuses the duplicate rather than
    // minting a second turn.
    if (pending === null || pending.text !== text) {
      pending = { key: crypto.randomUUID(), text };
    }
    inFlight = true;
    send.disabled = true;
    void send1(text, pending.key).finally(() => {
      inFlight = false;
      applyGate(opts.gate.current());
    });
  };

  async function send1(text: string, key: string): Promise<void> {
    clearRefusal(root);
    try {
      const response = await callUnary(
        ctx,
        "SubmitPrompt",
        (client) => client.submitPrompt(buildSubmitPromptRequest(ctx, text, key, opts.feed)),
        SubmitPromptResponseSchema,
      );
      const result = requireCase(response.result, "SubmitPromptResponse.result");
      if (result.case === "error") {
        drawSubmitRefusal(root, result.value.reason.case);
        log("warn", "SubmitPrompt was refused", {
          operation: "composer.refused",
          context: { arm: result.value.reason.case ?? "unset" },
        });
        return;
      }
      if (result.case !== "success") {
        const other: { case: string } = result;
        unreachableArm("SubmitPromptResponse.result", other.case);
      }
      const outcome = requireCase(result.value.outcome, "SubmitPromptSuccess.outcome");
      switch (outcome.case) {
        case "turn": {
          const turn = outcome.value.turn;
          if (turn === undefined) {
            // The one field the answer exists to carry. Refusing it loudly is
            // right, but the words are the user's — they stay in the box.
            drawSubmitRefusal(root, "unset");
            log("error", "SubmitPrompt answered a turn with no TurnId", {
              operation: "composer.turn-missing",
            });
            return;
          }
          lastTurn = turn;
          accepted();
          log("info", "SubmitPrompt minted a turn", {
            operation: "composer.submitted",
            context: { turn: turn.value },
          });
          return;
        }
        case "commandPanel":
          accepted();
          log("info", "SubmitPrompt answered a command panel", {
            operation: "composer.command-panel",
            context: { panel: outcome.value.panel.case ?? "unset" },
          });
          opts.onPanel(outcome.value);
          return;
        case "commandRefused":
          // NOT a refusal of the submission: the daemon recognized the command
          // and answered it with a feed row of its own, so the box is done with
          // these words. The card is the feed's, never this component's.
          accepted();
          log("info", `SubmitPrompt reported ${outcome.value.command} unsupported`, {
            operation: "composer.command-refused",
            context: { command: outcome.value.command },
          });
          return;
        default: {
          const other: { case: string } = outcome;
          unreachableArm("SubmitPromptSuccess.outcome", other.case);
        }
      }
    } catch (err) {
      drawSubmitRefusal(root, "transport");
      log("error", `SubmitPrompt failed at the transport: ${String(err)}`, {
        operation: "composer.transport-failure",
        context: { cause: err },
      });
    }
  }

  /** The submission landed: the words are the daemon's now, so the box clears. */
  function accepted(): void {
    input.value = "";
    pending = null;
  }

  const onKeyDown = (event: KeyboardEvent): void => {
    // Enter sends, Shift+Enter is a newline — the legacy composer's contract,
    // and the one every terminal-shaped box in this app trains the user for.
    if (event.key !== "Enter" || event.shiftKey) return;
    event.preventDefault();
    submit();
  };
  input.addEventListener("keydown", onKeyDown);
  send.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    submit();
  });

  /**
   * A prompt the user dropped from the hold tray hands its text back here.
   *
   * ONLY INTO AN EMPTY BOX. Restoring over a draft would destroy words the
   * user is in the middle of typing to fix words they deliberately discarded,
   * which trades one loss for a worse one.
   */
  const onDropped = (event: Event): void => {
    const detail = (event as CustomEvent<HeldPromptDroppedDetail>).detail;
    if (input.value !== "") {
      log("debug", "not restoring a dropped prompt over a draft", {
        operation: "composer.dropped-ignored",
      });
      return;
    }
    input.value = detail.text;
    log("info", "restored a dropped prompt's text", {
      operation: "composer.dropped-restored",
      context: { length: detail.text.length },
    });
  };
  document.addEventListener(DROPPED_EVENT, onDropped);

  return {
    lastTurn: () => lastTurn,
    dispose(): void {
      log("debug", "disposing a composer", { operation: "composer.dispose" });
      unsubscribeGate();
      document.removeEventListener(DROPPED_EVENT, onDropped);
      root.remove();
    },
  };
}

/**
 * The submission, built.
 *
 * `workspace` is REQUIRED on every submission — it names the workspace even
 * when `feed` is unset, because the root feed has no id of its own — and
 * `origin` is REQUIRED and is never UNSPECIFIED: this send site is the
 * webapp's own user-typed one, and the daemon persists that onto the turn's
 * durable record.
 */
export function buildSubmitPromptRequest(
  ctx: AppContext,
  text: string,
  idempotencyKey: string,
  feed?: FeedId,
): SubmitPromptRequest {
  return create(SubmitPromptRequestSchema, {
    workspace: ctx.workspace,
    said: buildUserSaid(text),
    idempotencyKey,
    origin: PromptOrigin.WEBAPP_USER_SENT,
    ...(feed !== undefined ? { feed } : {}),
  });
}

/** What the user typed, as the one canonical prompt form. */
export function buildUserSaid(text: string): UserSaid {
  return create(UserSaidSchema, {
    content: { blocks: [{ block: { case: "text", value: { text } } }] },
  });
}

/**
 * The refusal, drawn INLINE at the composer, per typed arm.
 *
 * The text is never touched: every one of these is a "not now" and the user
 * resubmits the same words once the state it named has resolved.
 */
export function drawSubmitRefusal(root: HTMLElement, arm: string | undefined): void {
  const named = arm ?? "unset";
  const refusal = document.createElement("div");
  refusal.className = "composer-refusal";
  refusal.setAttribute("data-arm", named);
  refusal.textContent = refusalText(named);
  root.appendChild(refusal);
}

/**
 * What each refusal says.
 *
 * `merging` is the ONE arm the contract declares, and its sentence is this
 * end's: the error message is empty on the wire on purpose (the footer and the
 * merge bubble already say which merge). Anything else — an arm a newer daemon
 * set, an unset reason, a transport failure — gets a sentence that is honest
 * about knowing nothing more, and the words stay in the box either way.
 */
function refusalText(arm: string): string {
  switch (arm) {
    case "merging":
      return "merge in flight — resubmit after it resolves";
    case "transport":
      return "the daemon could not be reached — your text is kept; try again";
    default:
      return "the daemon refused this submission — your text is kept; try again";
  }
}

function clearRefusal(root: HTMLElement): void {
  root.querySelectorAll(".composer-refusal").forEach((node) => node.remove());
}
