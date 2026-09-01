/**
 * The two link components, and the only two ways this page opens anything.
 *
 * WHY A COMPONENT RATHER THAN AN ANCHOR. This page IS the app, mounted inside
 * Emacs as an xwidget. A real navigation makes WebKit either take the webview
 * away from the conversation the user was reading or ask Emacs for a second
 * xwidget buffer. Both links therefore CANCEL the click before it can become a
 * navigation and turn it into an rpc — which is also what makes the
 * in-webview navigation impossible rather than merely unlikely: no navigation
 * is ever started, so there is no "navigate back afterwards" recovery to race.
 *
 * TWO DESTINATIONS, TWO VERBS.
 *   - `renderExternalLink` → `OpenExternal`: the daemon launches the pinned
 *     external browser profile.
 *   - `renderEditorLink` → `OpenInEditor`: the daemon RELAYS the click to
 *     Emacs on the workspace's host stream, which answers with its one shared
 *     editor-popup subroutine. The daemon opens nothing itself and nothing
 *     acks — the push is a fact ("the user clicked this").
 *
 * `renderEditorLink` is the ONE shared jump-to-file affordance: the plan
 * bubble's edit button, a findings row's location and a worktree divider's
 * path all render through it. A second one would be a defect.
 *
 * NOTHING IS DRAWN ON SUCCESS. The result of either click happens elsewhere —
 * in a browser window, in an editor — so a confirmation on this page would be
 * chrome reporting something the user is already looking at. A REFUSAL is
 * drawn, at the link itself, per the call-site rule.
 */
import { log } from "./log.js";
import {
  OpenExternalResponseSchema,
  type OpenExternalError,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import {
  OpenInEditorResponseSchema,
  type OpenInEditorError,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import type { AppContext } from "./rpc/context.js";
import { isMalformedView } from "./rpc/malformed.js";
import { refusalSentence } from "./rpc/refusal.js";
import { requireCase, unreachableArm } from "./rpc/strict.js";
import { callUnary } from "./rpc/unary.js";

/** Only these two schemes are a hyperlink; everything else is text. */
const LINKABLE_SCHEME = /^https?:\/\//i;

export interface ExternalLinkSpec {
  /** The visible text. Empty falls back to the url, so a link is never blank. */
  text: string;
  /** The destination, verbatim as the view carried it. */
  url: string;
}

export interface EditorLinkSpec {
  /** The visible text (a file name, a path, "edit"). */
  text: string;
  /** The path on the daemon's HOST, exactly as the feed row carried it. */
  path: string;
  /** The 1-indexed line to land on. Unset = the file's top, or a directory. */
  line?: number;
}

/**
 * A link to somewhere outside this page.
 *
 * ONLY http(s) IS LINKABLE. Anything else — a `mailto:`, a bare path, a
 * `javascript:` — renders as PLAIN TEXT with a warning rather than as a link
 * that would do nothing when clicked or, worse, something unintended. The
 * `href` is still set on the linkable case so the destination shows in a
 * hover and a copy-link gesture works; the click never follows it.
 */
export function renderExternalLink(ctx: AppContext, spec: ExternalLinkSpec): HTMLElement {
  const label = spec.text === "" ? spec.url : spec.text;
  if (!LINKABLE_SCHEME.test(spec.url)) {
    log("warn", `refusing to link a non-http(s) destination; drawing it as text`, {
      operation: "link.unlinkable-scheme",
      context: { url: spec.url },
    });
    const plain = document.createElement("span");
    plain.className = "external-link unlinkable";
    plain.textContent = label;
    return plain;
  }

  const anchor = document.createElement("a");
  anchor.className = "external-link";
  anchor.href = spec.url;
  anchor.textContent = label;
  anchor.addEventListener("click", (event: MouseEvent) => {
    if (!claimsClick(event)) return;
    // Cancel FIRST. A failed open is reported below, while a navigation would
    // already have destroyed the page the report was going to appear on.
    event.preventDefault();
    event.stopPropagation();
    void openExternal(ctx, anchor, spec.url);
  });
  return anchor;
}

/**
 * A link that opens something in the user's editor.
 *
 * PLAIN FIELDS, ECHOED. The path is relayed verbatim — the daemon validates
 * the workspace and forwards the string without interpreting it — and `line`
 * is set only when the view gave one, because an unset line means the file's
 * top and a zero would be a sentinel claiming line zero exists.
 */
export function renderEditorLink(ctx: AppContext, spec: EditorLinkSpec): HTMLElement {
  const anchor = document.createElement("a");
  anchor.className = "editor-link";
  anchor.textContent = spec.text === "" ? spec.path : spec.text;
  // The stable hook every jump-to-file affordance carries, whichever view
  // drew it: the plan button, a finding's location, a worktree divider's path.
  anchor.setAttribute("data-editor-link", "");
  anchor.setAttribute("data-host-path", spec.path);
  if (spec.line !== undefined) anchor.setAttribute("data-host-line", String(spec.line));
  // No href: the destination is on the daemon's host, so there is no URL a
  // browser could meaningfully show or copy. The cursor is the stylesheet's.
  anchor.setAttribute("role", "button");
  anchor.tabIndex = 0;
  anchor.addEventListener("click", (event: MouseEvent) => {
    if (!claimsClick(event)) return;
    event.preventDefault();
    event.stopPropagation();
    void openInEditor(ctx, anchor, spec);
  });
  return anchor;
}

/**
 * Whether this component takes the click.
 *
 * A modified or non-primary click is the user asking their own platform to do
 * something with the link, and an already-cancelled event has been claimed by
 * somebody else. Neither is ours to hijack.
 */
function claimsClick(event: MouseEvent): boolean {
  if (event.defaultPrevented) return false;
  if (event.button !== 0) return false;
  return !(event.metaKey || event.ctrlKey || event.shiftKey || event.altKey);
}

async function openExternal(ctx: AppContext, anchor: HTMLElement, url: string): Promise<void> {
  clearRefusal(anchor);
  try {
    const response = await callUnary(
      ctx,
      "OpenExternal",
      (client) => client.openExternal({ workspace: ctx.workspace, url }),
      OpenExternalResponseSchema,
    );
    const result = requireCase(response.result, "OpenExternalResponse.result");
    if (result.case === "success") return;
    const cause = requireCase(
      (result.value as OpenExternalError).cause,
      "OpenExternalError.cause",
    );
    const say = refusalSentence("OpenExternal", cause) ?? openExternalRefusal(cause);
    drawRefusal(anchor, cause.case, say);
    log("warn", `OpenExternal refused ${url}`, {
      operation: "link.open-external-refused",
      context: { url, arm: cause.case, sentence: say },
    });
  } catch (err) {
    // A link that went nowhere must say so: the user just clicked expecting a
    // browser window, and silence would read as a dead rail.
    if (isMalformedView(err)) throw err;
    drawRefusal(anchor, "transport", "the daemon could not be reached");
    log("error", `OpenExternal failed for ${url}: ${String(err)}`, {
      operation: "link.open-external-failed",
      context: { url, cause: err },
    });
  }
}

async function openInEditor(ctx: AppContext, anchor: HTMLElement, spec: EditorLinkSpec): Promise<void> {
  clearRefusal(anchor);
  try {
    const response = await callUnary(
      ctx,
      "OpenInEditor",
      (client) =>
        client.openInEditor({
          workspace: ctx.workspace,
          path: spec.path,
          ...(spec.line !== undefined ? { line: spec.line } : {}),
        }),
      OpenInEditorResponseSchema,
    );
    const result = requireCase(response.result, "OpenInEditorResponse.result");
    if (result.case === "success") return;
    const cause = requireCase(
      (result.value as OpenInEditorError).cause,
      "OpenInEditorError.cause",
    );
    const say = refusalSentence("OpenInEditor", cause) ?? openInEditorRefusal(cause);
    drawRefusal(anchor, cause.case, say);
    log("warn", `OpenInEditor refused ${spec.path}`, {
      operation: "link.open-in-editor-refused",
      context: { path: spec.path, line: spec.line, arm: cause.case, sentence: say },
    });
  } catch (err) {
    if (isMalformedView(err)) throw err;
    drawRefusal(anchor, "transport", "the daemon could not be reached");
    log("error", `OpenInEditor failed for ${spec.path}: ${String(err)}`, {
      operation: "link.open-in-editor-failed",
      context: { path: spec.path, line: spec.line, cause: err },
    });
  }
}

/**
 * The refusal, AT THE CALL SITE.
 *
 * A `<Method>Error` is an answer to this click and belongs on the control that
 * was clicked — never as pushed state, which would put it somewhere the user
 * is not looking. The anchor carries it as a `title` (the hover the platform
 * already gives) and as the shared `.refusal[data-arm]` marker the integration
 * suite targets.
 */
function drawRefusal(anchor: HTMLElement, arm: string, message: string): void {
  anchor.classList.add("refusal");
  anchor.setAttribute("data-arm", arm);
  anchor.title = message;
}

/** `OpenExternalError`'s cause union, narrowed to a SET arm. */
type OpenExternalCause = NonNullable<OpenExternalError["cause"]> & { case: string };

/** `OpenInEditorError`'s cause union, narrowed to a SET arm. */
type OpenInEditorCause = NonNullable<OpenInEditorError["cause"]> & { case: string };

/**
 * OpenExternal's own three arms.
 *
 * The url is NOT restated in the sentence — the anchor the refusal lands on is
 * the url — so each says the one thing the anchor cannot: whether the address
 * was rejected, whether the host has no browser to open it with, or what the
 * launch itself failed on.
 */
export function openExternalRefusal(cause: OpenExternalCause): string {
  switch (cause.case) {
    case "invalidUrl":
      return "the daemon would not accept this address";
    case "noBrowserConfigured":
      return "the host has no browser configured to open links with";
    case "launchFailed":
      return `the browser could not be launched: ${cause.value.detail}`;
    default: {
      const other: { case: string } = cause;
      return unreachableArm("OpenExternalError.cause", other.case);
    }
  }
}

/** OpenInEditor's own arm: the path guard the daemon applies. */
export function openInEditorRefusal(cause: OpenInEditorCause): string {
  switch (cause.case) {
    case "pathEscapesWorkspace":
      return "that path is outside this workspace";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("OpenInEditorError.cause", other.case);
    }
  }
}

function clearRefusal(anchor: HTMLElement): void {
  anchor.classList.remove("refusal");
  anchor.removeAttribute("data-arm");
  anchor.removeAttribute("title");
}
