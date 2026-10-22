/**
 * ingress-prompt — a prompt waiting in the held-prompt ingress, drawn as a
 * parked card.
 *
 * A prompt sent while no daemon answered is written by the editor to the
 * durable held-prompt ingress, and is in no queue until a daemon accepts it.
 * Without a card it would be drawn nowhere, and a person who sees nothing sends
 * it again (2026-10-20). So it is drawn exactly as a held prompt is (the one
 * bubble, `held` variant, the prompt rail), with ONE badge saying why it waits
 * and its queued age, and NO controls: nothing can force, cancel, edit or fold
 * a prompt no queue holds.
 *
 * TWO SOURCES, NEVER AT ONCE (held/section.ts). While the page's root feed is
 * live the daemon draws these (`FeedHeldItem.ingress`), its badge composed
 * daemon-side. While the daemon is out of reach the editor's own list is drawn
 * (`EditorIngressEntry`, host/protocol.ts `heldIngress`), and its badge is the
 * page's: the page alone knows its link is down, the one deliberate exception
 * to the stateless renderer rule the footer's client verdict also takes
 * (rpc/link.ts).
 */
import type {
  HeldIngressPrompt,
  HeldIngressPromptBadge,
} from "../../../proto/gen/ts/frontend/v1/feed_held_pb";
import { BUBBLE_MORE_ELLIPSIS, ELLIPSIS_CAP_LINES, drawBubble } from "../bubble/draw.js";
import { log } from "../log.js";
import { MalformedView } from "../rpc/malformed.js";
import { msOf, requireCase, requireMessage } from "../rpc/strict.js";
import type { HeldContext } from "./context.js";
import type { EditorIngressEntry } from "./editor-ingress.js";
import { drawHeldBadgePill, drawQueuedAge, drawUserSaid, type HeldBadgeTone } from "./held-prompt.js";
import { drawFeedUserPromptBody } from "../feed/rows/user-prompt.js";

/** The attribute an ingress card carries: who drew it, `daemon` or `editor`. */
export const INGRESS_SOURCE_ATTRIBUTE = "data-held-ingress";

/** What an ingress card's badge stands for: the daemon's arms, and the page's own. */
export type IngressStatus = NonNullable<HeldIngressPromptBadge["standsFor"]["case"]> | "daemonUnreachable";

/**
 * Each status's badge color, the held badges' palette (held-prompt.ts):
 * waiting on nothing in particular, muted; on a session or a daemon coming
 * up, the hibernation teal; on coordinated daemon work, amber; on a fault,
 * the error red.
 */
export const INGRESS_STATUS_BADGES = {
  awaitingIntake: "muted",
  noSession: "teal",
  merge: "amber",
  coldGate: "teal",
  movedAway: "amber",
  refused: "err",
  daemonUnreachable: "teal",
} as const satisfies Record<IngressStatus, HeldBadgeTone>;

/** The page's own badge words for an editor entry: the daemon is out of reach. */
export const DAEMON_UNREACHABLE_LABEL = "waiting for the daemon";

/** What one ingress card draws, whichever source it came from. */
interface IngressCard {
  readonly source: "daemon" | "editor";
  /**
   * What the card draws: the daemon's resolved body for its own item, or the
   * editor's raw said for an entry the editor holds while the daemon is out
   * of reach (no daemon resolved it).
   */
  readonly content: HTMLElement[];
  readonly queuedAtMs: number;
  readonly label: string;
  readonly detail: string | null;
  readonly status: IngressStatus;
}

/** The daemon's ingress item, drawn over PREVIOUS when given. */
export function drawHeldIngressPrompt(u: HeldIngressPrompt, tc: HeldContext, previous?: HTMLElement): HTMLElement {
  const path = "HeldIngressPrompt";
  const badge = requireMessage(u.badge, `${path}.badge`);
  const status = requireCase(badge.standsFor, `${path}.badge.stands_for`).case;
  if (badge.label === "") {
    log.error("an ingress prompt's badge carried an empty label", {
      operation: "held.ingress-prompt.badge-empty-label",
      context: { path, status },
    });
    throw new MalformedView(`${path}.badge.label`, "the badge label is empty");
  }
  return drawIngressCard(
    {
      source: "daemon",
      content: drawFeedUserPromptBody(requireMessage(u.body, `${path}.body`), `${path}.body`),
      queuedAtMs: msOf(requireMessage(u.queuedAt, `${path}.queued_at`).atMs, `${path}.queued_at.at_ms`),
      label: badge.label,
      detail: badge.detail ?? null,
      status,
    },
    tc,
    path,
    previous,
  );
}

/** One of the editor's own ingress entries, drawn while the daemon is out of reach. */
export function drawEditorIngressEntry(entry: EditorIngressEntry, tc: HeldContext, previous?: HTMLElement): HTMLElement {
  return drawIngressCard(
    {
      source: "editor",
      content: drawUserSaid(entry.said, "EditorIngressEntry.said"),
      queuedAtMs: entry.queuedAtMs,
      label: DAEMON_UNREACHABLE_LABEL,
      detail: null,
      status: "daemonUnreachable",
    },
    tc,
    "EditorIngressEntry",
    previous,
  );
}

/** The card itself: the one bubble, its badge and age, and what was said. */
function drawIngressCard(card: IngressCard, tc: HeldContext, path: string, previous?: HTMLElement): HTMLElement {
  log.debug("drawing an ingress prompt", {
    operation: "held.ingress-prompt",
    context: { source: card.source, status: card.status },
  });
  const head = document.createElement("div");
  head.className = "queued-head";
  const pill = drawHeldBadgePill(card.label, INGRESS_STATUS_BADGES[card.status], card.status);
  if (card.detail !== null) pill.title = card.detail;
  head.append(pill, drawQueuedAge(card.queuedAtMs, tc, `${path}.queued_at`));
  const element = drawBubble(
    {
      role: "prompt",
      variant: "held",
      hooks: ["held-right"],
      waving: false,
      strip: [head],
      content: card.content,
      capLines: ELLIPSIS_CAP_LINES,
      more: BUBBLE_MORE_ELLIPSIS,
    },
    previous,
  ).bubble;
  element.setAttribute(INGRESS_SOURCE_ATTRIBUTE, card.source);
  return element;
}
