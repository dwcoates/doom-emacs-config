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
 * `renderMergeTestLogLink` is the same verb with the other target: a merge's
 * test log, named by the opaque token the merge bubble served, because the
 * log lives in the daemon's state and has no workspace path to send.
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
import type { FeedMergeTestLog, FeedMergeTestLogToken } from "../../proto/gen/ts/frontend/v1/feed_pb";
import type { AppContext } from "./rpc/context.js";
import {
  clearRefusals,
  drawMalformedRefusal,
  drawTransportRefusal,
  drawTypedRefusal,
  type SentenceTable,
} from "./rpc/refuse.js";
import { requireCase, requireMessage, unreachableArm } from "./rpc/strict.js";
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
    log.warn(`refusing to link a non-http(s) destination; drawing it as text`, {
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
  // The stable hook every outward link carries, whichever view drew it — the
  // twin of `data-editor-link` below.
  anchor.setAttribute("data-external-link", "");
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
    void openInEditor(ctx, anchor, workspaceFileTarget(spec));
  });
  return anchor;
}

/**
 * A merge's test log, drawn in the link blue a response bubble's links wear.
 *
 * THE TOKEN IS ECHOED, NEVER READ. The merge bubble served it and
 * `OpenInEditorRequest.merge_test_log` takes it back unchanged; the page never
 * parses it, builds one, or shows it. What the reader sees is the label the
 * daemon composed (the log's path, shortened with ~).
 */
export function renderMergeTestLogLink(
  ctx: AppContext,
  u: FeedMergeTestLog,
  path: string,
): HTMLElement {
  const token = requireMessage(u.token, `${path}.token`);
  const label = requireMessage(u.label, `${path}.label`);
  const anchor = document.createElement("a");
  anchor.className = "editor-link merge-test-log-link";
  anchor.textContent = label.text;
  anchor.setAttribute("data-merge-test-log", "");
  // No href, for the same reason as the editor link: the log is on the
  // daemon's host, and the click is an rpc, never a navigation.
  anchor.setAttribute("role", "button");
  anchor.tabIndex = 0;
  anchor.addEventListener("click", (event: MouseEvent) => {
    if (!claimsClick(event)) return;
    event.preventDefault();
    event.stopPropagation();
    void openInEditor(ctx, anchor, { case: "mergeTestLog", value: token });
  });
  return anchor;
}

/** What an editor link opens: a workspace file, or a merge's test log. */
export type EditorTarget =
  | { case: "workspaceFile"; value: { path: string; line?: number } }
  | { case: "mergeTestLog"; value: FeedMergeTestLogToken };

/** A workspace file's target, with the line only when the view gave one. */
export function workspaceFileTarget(spec: { path: string; line?: number }): EditorTarget {
  return {
    case: "workspaceFile",
    value: { path: spec.path, ...(spec.line !== undefined ? { line: spec.line } : {}) },
  };
}

/** What a log line says the click was opening, per target. */
function editorTargetContext(target: EditorTarget): Record<string, unknown> {
  switch (target.case) {
    case "workspaceFile":
      return { target: target.case, path: target.value.path, line: target.value.line };
    case "mergeTestLog":
      return { target: target.case, token: target.value.value };
    default: {
      const other: { case: string } = target;
      return unreachableArm("OpenInEditorRequest.target", other.case);
    }
  }
}

/**
 * Route a clicked PROSE link — a markdown anchor in a feed bubble or the hold
 * tray — through the SAME two verbs the structured links use, so a link in
 * model prose never navigates the webview either.
 *
 * ONE DELEGATED INTERCEPTOR, hung at the feed/markdown seam (`feed-scroll`,
 * which holds every bubble and the hold tray) rather than on each rendered
 * anchor: the markdown is injected as innerHTML with no per-anchor handler, so
 * a single capture-phase listener is the whole of it. It runs in CAPTURE so it
 * beats a bubble's own click affordances, and cancels the click BEFORE it can
 * become a WebKit navigation.
 *
 * IT LEAVES STRUCTURED LINKS ALONE. `renderExternalLink`'s anchor carries
 * `data-external-link` and already routes itself; a prose anchor carries
 * neither hook. Skipping the marked ones — WITHOUT cancelling, so their own
 * bubble-phase handler still runs — is what keeps this from double-firing the
 * rpc a structured link makes. `renderEditorLink`'s anchor has no `href` at
 * all, so `a[href]` never selects it.
 *
 * FILE vs WEB IS THE SCHEME. http(s) is a web link and opens externally;
 * `file://` or a filesystem path is a local file and opens in the editor. A
 * scheme that is neither is left untouched (markdown emits only http(s)
 * anchors, so this is a guard, not a path).
 */
export function installProseLinkRouting(ctx: AppContext, root: HTMLElement): () => void {
  const onClick = (event: MouseEvent): void => routeProseLinkClick(ctx, event);
  root.addEventListener("click", onClick, true);
  return () => root.removeEventListener("click", onClick, true);
}

function routeProseLinkClick(ctx: AppContext, event: MouseEvent): void {
  const target = event.target;
  if (!(target instanceof Element)) return;
  const anchor = target.closest("a[href]");
  if (!(anchor instanceof HTMLElement)) return;
  // The structured links own their own click; never handle them twice, and
  // never cancel here — their bubble-phase handler still has to fire.
  if (anchor.hasAttribute("data-external-link") || anchor.hasAttribute("data-editor-link")) return;
  if (!claimsClick(event)) return;

  // The RAW href, exactly as the markdown carried it: the resolved `.href`
  // would have turned a relative or `file://` path into a page-origin url.
  const href = anchor.getAttribute("href") ?? "";
  if (LINKABLE_SCHEME.test(href)) {
    // Cancel FIRST — a navigation would destroy the page a refusal draws on.
    event.preventDefault();
    event.stopPropagation();
    void openExternal(ctx, anchor, href);
    return;
  }
  const path = localFilePath(href);
  if (path !== null) {
    event.preventDefault();
    event.stopPropagation();
    void openInEditor(ctx, anchor, workspaceFileTarget({ path }));
    return;
  }
  // Neither web nor local file. Markdown restricts anchors to http/https, so
  // this is unreachable from rendered prose; it is logged rather than silently
  // cancelled so a scheme that ever does slip through is visible, not dead.
  log.warn(`ignoring a prose link with an unroutable scheme`, {
    operation: "link.prose-unroutable-scheme",
    context: { href },
  });
}

/**
 * The filesystem path a prose href names, or null when it is not a local file.
 *
 * A `file://` url's path is its decoded pathname; a bare absolute path (`/…`)
 * or a home path (`~/…`) is itself. Everything else — including a relative
 * fragment or a query — is not a file this can open.
 */
function localFilePath(href: string): string | null {
  if (/^file:\/\//i.test(href)) {
    try {
      return decodeURIComponent(new URL(href).pathname);
    } catch {
      return null;
    }
  }
  if (href.startsWith("/") || href.startsWith("~/")) return href;
  return null;
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
    const arm = drawTypedRefusal(
      refusalHost(anchor),
      "OpenExternalError.cause",
      "OpenExternal",
      (result.value).cause,
      EXTERNAL_SENTENCES,
    );
    log.warn(`OpenExternal refused ${url}`, {
      operation: "link.open-external-refused",
      context: { url, arm },
    });
  } catch (err) {
    // A link that went nowhere must say so: the user just clicked expecting a
    // browser window, and silence would read as a dead rail.
    if (drawMalformedRefusal(ctx, refusalHost(anchor), "link.open-external-unreadable", err)) {
      return;
    }
    drawTransportRefusal(refusalHost(anchor));
    log.error(`OpenExternal failed for ${url}: ${String(err)}`, {
      operation: "link.open-external-failed",
      context: { url, cause: err },
    });
  }
}

async function openInEditor(ctx: AppContext, anchor: HTMLElement, target: EditorTarget): Promise<void> {
  clearRefusal(anchor);
  const context = editorTargetContext(target);
  try {
    const response = await callUnary(
      ctx,
      "OpenInEditor",
      (client) => client.openInEditor({ workspace: ctx.workspace, target }),
      OpenInEditorResponseSchema,
    );
    const result = requireCase(response.result, "OpenInEditorResponse.result");
    if (result.case === "success") return;
    const arm = drawTypedRefusal(
      refusalHost(anchor),
      "OpenInEditorError.cause",
      "OpenInEditor",
      (result.value).cause,
      EDITOR_SENTENCES,
    );
    log.warn(`OpenInEditor refused a ${target.case} target`, {
      operation: "link.open-in-editor-refused",
      context: { ...context, arm },
    });
  } catch (err) {
    if (drawMalformedRefusal(ctx, refusalHost(anchor), "link.open-in-editor-unreadable", err)) {
      return;
    }
    drawTransportRefusal(refusalHost(anchor));
    log.error(`OpenInEditor failed for a ${target.case} target: ${String(err)}`, {
      operation: "link.open-in-editor-failed",
      context: { ...context, cause: err },
    });
  }
}

/**
 * Where a link's refusal is drawn.
 *
 * BESIDE the anchor, not on it: the sentence carries the arm's own facts (the
 * registry dir, a successor's address) and an anchor whose text is a url has
 * nowhere to put them. The anchor's parent is the row or line the link sits
 * in, so the refusal lands as its next sibling exactly as the hook contract
 * says; a detached anchor keeps it inside itself, which is still at the call
 * site.
 */
function refusalHost(anchor: HTMLElement): HTMLElement {
  return anchor.parentElement ?? anchor;
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

/** OpenInEditor's own arms: the path guard, and a log token it does not hold. */
export function openInEditorRefusal(cause: OpenInEditorCause): string {
  switch (cause.case) {
    case "pathEscapesWorkspace":
      return "that path is outside this workspace";
    case "unknownMergeTestLog":
      return "that test log is no longer available";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("OpenInEditorError.cause", other.case);
    }
  }
}

/**
 * The two endpoints' OWN arms, in the shape the one refusal hook takes.
 *
 * The cross-cutting four are worded once in `rpc/refusal.ts` and never
 * repeated here; these tables carry only what each verb alone can mean.
 */
const EXTERNAL_SENTENCES: SentenceTable = {
  invalidUrl: () => openExternalRefusal({ case: "invalidUrl", value: {} } as OpenExternalCause),
  noBrowserConfigured: () =>
    openExternalRefusal({ case: "noBrowserConfigured", value: {} } as OpenExternalCause),
  launchFailed: (value) =>
    openExternalRefusal({ case: "launchFailed", value }),
};

const EDITOR_SENTENCES: SentenceTable = {
  pathEscapesWorkspace: () =>
    openInEditorRefusal({ case: "pathEscapesWorkspace", value: {} } as OpenInEditorCause),
  unknownMergeTestLog: () =>
    openInEditorRefusal({ case: "unknownMergeTestLog", value: {} } as OpenInEditorCause),
};

function clearRefusal(anchor: HTMLElement): void {
  clearRefusals(refusalHost(anchor));
}
