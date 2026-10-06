/**
 * news-digest — the daily Claude news digest, drawn as one large overlay over
 * the FEED (never the topbar, the rail or the footer) in every webview while
 * it stands.
 *
 * THE DAEMON WRITES THE DIGEST, AND THIS DRAWS IT VERBATIM. The standing
 * arrives on the page's `WatchDaemon` stream (`NewsDigestStanding`, a webview
 * stream's alone): `shown` replaces whatever was drawn with the whole overlay,
 * `none` takes it down. Nothing here orders, counts, words or filters the
 * news; every title, summary, heading and source row is the daemon's.
 *
 * A DISMISS IS AN RPC, and only the daemon's push takes the overlay down. The
 * close control and Escape call `DismissNewsDigest` with the id the overlay
 * was served with; the daemon then pushes `none` to EVERY webview, this one
 * included, which is what removes it. A refusal (`unknown_digest`: a newer
 * digest stands) is drawn at the close control, per the call-site rule.
 *
 * THE OVERLAY COVERS THE FEED'S OWN BOX. The host is fixed-positioned and
 * follows the scroll zone's rectangle (a ResizeObserver on it, and the
 * window's resize), so the feed underneath is never moved, hidden or
 * re-laid out: its scroll position and its anchoring are exactly where the
 * reader left them when the overlay comes down.
 */
import {
  DismissNewsDigestResponseSchema,
  type DismissNewsDigestError,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_dismiss_news_digest_pb";
import type { NewsDigestStanding } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import type {
  NewsDigestItem,
  NewsDigestOverlay,
  NewsDigestSection,
  NewsDigestSource,
} from "../../../proto/gen/ts/frontend/v1/news_digest_pb";
import { createControl } from "../control.js";
import { escapeHtml } from "../highlight.js";
import { installProseLinkRouting, renderExternalLink } from "../link.js";
import { inline, renderMarkdown } from "../markdown.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { MalformedView } from "../rpc/malformed.js";
import {
  clearRefusals,
  drawTransportRefusal,
  drawTypedRefusal,
  type SentenceTable,
} from "../rpc/refuse.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";

/** What the mount answers with. */
export interface NewsDigestHandle {
  /** Draw one pushed standing: the whole overlay, or none. */
  apply(standing: NewsDigestStanding): void;
  dispose(): void;
}

export interface NewsDigestDeps {
  /** The scroll zone the overlay covers (`#feed-scroll`). */
  feedScroll: HTMLElement;
}

/** The causes only DismissNewsDigest can answer with. */
export const DISMISS_NEWS_DIGEST_CAUSES = {
  unknownDigest: () => "a newer digest has replaced this one",
} as unknown as SentenceTable;

/** The words on the close control, which are also its accessible name. */
export const CLOSE_LABEL = "close";

/**
 * Mount the overlay on HOST. It ships hidden and draws nothing until a
 * `shown` standing arrives.
 */
export function mountNewsDigest(
  host: HTMLElement,
  ctx: AppContext,
  deps: NewsDigestDeps,
): NewsDigestHandle {
  log.debug("mounting the news digest overlay", { operation: "news-digest.mount" });
  host.hidden = true;
  host.replaceChildren();

  /** The id the drawn overlay was served with; null while none is drawn. */
  let drawnId: string | null = null;
  /** The control a dismiss's refusal is drawn beside. */
  let actions: HTMLElement | null = null;

  const follow = (): void => {
    if (host.hidden) return;
    const rect = deps.feedScroll.getBoundingClientRect();
    host.style.top = `${rect.top}px`;
    host.style.left = `${rect.left}px`;
    host.style.width = `${rect.width}px`;
    host.style.height = `${rect.height}px`;
  };
  // A LINK IN A SUMMARY ROUTES AS ONE IN A RESPONSE BUBBLE: the digest's
  // markdown is drawn by the same renderer, so its anchors take the same route.
  const unrouteLinks = installProseLinkRouting(ctx, host);
  const observer = new ResizeObserver(follow);
  observer.observe(deps.feedScroll);
  window.addEventListener("resize", follow);

  const dismiss = (): void => {
    if (drawnId === null || actions === null) return;
    void guardMalformed(ctx, "news-digest.dismiss", dismissNewsDigest(ctx, actions, drawnId));
  };

  const onKeydown = (event: KeyboardEvent): void => {
    if (event.key !== "Escape" || drawnId === null) return;
    event.preventDefault();
    log.debug("Escape dismisses the news digest", { operation: "news-digest.escape" });
    dismiss();
  };
  document.addEventListener("keydown", onKeydown);

  const show = (overlay: NewsDigestOverlay): void => {
    const drawn = drawOverlay(ctx, overlay, dismiss);
    drawnId = drawn.id;
    actions = drawn.actions;
    host.replaceChildren(drawn.panel);
    host.hidden = false;
    follow();
    log.info("the news digest stands; drawing it over the feed", {
      operation: "news-digest.shown",
      context: { sections: overlay.sections.length },
    });
  };

  const hide = (): void => {
    if (drawnId === null) return;
    drawnId = null;
    actions = null;
    host.hidden = true;
    host.replaceChildren();
    log.info("no news digest stands; taking the overlay down", { operation: "news-digest.none" });
  };

  return {
    apply(standing: NewsDigestStanding): void {
      const arm = requireCase(standing.standing, "NewsDigestStanding.standing");
      switch (arm.case) {
        case "shown":
          show(arm.value);
          return;
        case "none":
          hide();
          return;
        default: {
          const other: { case: string } = arm;
          return unreachableArm("NewsDigestStanding.standing", other.case);
        }
      }
    },
    dispose(): void {
      log.debug("disposing the news digest overlay", { operation: "news-digest.dispose" });
      unrouteLinks();
      observer.disconnect();
      window.removeEventListener("resize", follow);
      document.removeEventListener("keydown", onKeydown);
      drawnId = null;
      actions = null;
      host.hidden = true;
      host.replaceChildren();
    },
  };
}

/**
 * Ask the daemon to take the digest ID down in every webview. Success draws
 * nothing: the daemon's `none` push is what removes the overlay. A refusal is
 * drawn in ACTIONS, beside the close control.
 */
export async function dismissNewsDigest(ctx: AppContext, actions: HTMLElement, id: string): Promise<void> {
  log.info("dismissing the news digest", { operation: "news-digest.dismiss" });
  clearRefusals(actions);
  let response;
  try {
    response = await callUnary(
      ctx,
      "DismissNewsDigest",
      (client) => client.dismissNewsDigest({ id: { value: id } }),
      DismissNewsDigestResponseSchema,
    );
  } catch (err) {
    drawTransportRefusal(actions, err);
    return;
  }
  const result = requireCase(response.result, "DismissNewsDigestResponse.result");
  switch (result.case) {
    case "success":
      log.debug("the daemon took the news digest down; its push removes the overlay", {
        operation: "news-digest.dismissed",
      });
      return;
    case "error":
      drawDismissRefusal(actions, result.value);
      return;
    default: {
      const other: { case: string } = result;
      return unreachableArm("DismissNewsDigestResponse.result", other.case);
    }
  }
}

function drawDismissRefusal(actions: HTMLElement, error: DismissNewsDigestError): void {
  drawTypedRefusal(
    actions,
    "DismissNewsDigestError.cause",
    "DismissNewsDigest",
    error.cause,
    DISMISS_NEWS_DIGEST_CAUSES,
  );
}

/**
 * The whole overlay for one served digest: the panel, the id it echoes, and
 * the actions host a refusal is drawn in. Throws `MalformedView` for any
 * element the contract requires and the push did not carry.
 */
export function drawOverlay(
  ctx: AppContext,
  overlay: NewsDigestOverlay,
  dismiss: () => void,
): { panel: HTMLElement; id: string; actions: HTMLElement } {
  const id = requireMessage(overlay.id, "NewsDigestOverlay.id").value;
  if (id === "") throw new MalformedView("NewsDigestOverlay.id.value", "a digest's id is required");
  const header = requireMessage(overlay.header, "NewsDigestOverlay.header");
  const title = requireMessage(header.title, "NewsDigestHeader.title");
  const period = requireMessage(header.period, "NewsDigestHeader.period");
  const sources = requireMessage(overlay.sources, "NewsDigestOverlay.sources");

  const panel = document.createElement("div");
  panel.className = "news-digest-panel";
  panel.setAttribute("role", "dialog");
  panel.setAttribute("aria-label", title.text);

  const head = document.createElement("div");
  head.className = "news-digest-header";
  const heading = document.createElement("div");
  heading.className = "news-digest-heading";
  const titleElement = document.createElement("div");
  titleElement.className = "news-digest-title";
  titleElement.textContent = title.text;
  const periodElement = document.createElement("div");
  periodElement.className = "news-digest-period";
  periodElement.textContent = formatPeriod(
    msOf(period.fromMs, "NewsDigestPeriod.from_ms"),
    msOf(period.toMs, "NewsDigestPeriod.to_ms"),
  );
  heading.append(titleElement, periodElement);
  const actions = document.createElement("div");
  actions.className = "news-digest-actions";
  const close = createControl();
  close.className = "news-digest-close";
  close.setAttribute("data-news-digest-close", "");
  close.setAttribute("aria-label", CLOSE_LABEL);
  close.textContent = CLOSE_LABEL;
  close.addEventListener("click", dismiss);
  actions.append(close);
  head.append(heading, actions);

  const body = document.createElement("div");
  body.className = "news-digest-body";
  overlay.sections.forEach((section, i) => body.append(drawSection(ctx, section, `NewsDigestOverlay.sections[${i}]`)));

  const footer = document.createElement("div");
  footer.className = "news-digest-sources";
  sources.sources.forEach((source, i) =>
    footer.append(drawSource(ctx, source, `NewsDigestSources.sources[${i}]`)),
  );

  panel.append(head, body, footer);
  return { panel, id, actions };
}

/**
 * One section: its heading and its items. The kind arm is the section's
 * `data-kind`, which is the whole of its accent (the backend kind is drawn as
 * a warning by the stylesheet).
 */
export function drawSection(ctx: AppContext, section: NewsDigestSection, path: string): HTMLElement {
  const heading = requireMessage(section.heading, `${path}.heading`);
  const kind = requireCase(requireMessage(section.kind, `${path}.kind`).kind, `${path}.kind.kind`);
  switch (kind.case) {
    case "backend":
    case "deprecation":
    case "policy":
    case "feature":
    case "release":
    case "incident":
      break;
    default: {
      const other: { case: string } = kind;
      return unreachableArm(`${path}.kind.kind`, other.case);
    }
  }
  if (section.items.length === 0) throw new MalformedView(`${path}.items`, "a section is never empty");
  const element = document.createElement("section");
  element.className = "news-digest-section";
  element.setAttribute("data-kind", kind.case);
  const headingElement = document.createElement("h2");
  headingElement.className = "news-digest-section-heading";
  headingElement.textContent = heading.text;
  element.append(headingElement);
  section.items.forEach((item, i) => element.append(drawItem(ctx, item, `${path}.items[${i}]`)));
  return element;
}

/** One item: title, the effective date when one is stated, summary, links. */
export function drawItem(ctx: AppContext, item: NewsDigestItem, path: string): HTMLElement {
  const title = requireMessage(item.title, `${path}.title`);
  const summary = requireMessage(item.summary, `${path}.summary`);
  if (item.links.length === 0) throw new MalformedView(`${path}.links`, "an item always links to its source");
  const element = document.createElement("article");
  element.className = "news-digest-item";
  const head = document.createElement("div");
  head.className = "news-digest-item-head";
  const titleElement = document.createElement("span");
  titleElement.className = "news-digest-item-title";
  // MARKDOWN, as a response bubble draws it (owner, 2026-10-06): the title
  // takes inline markup only, the summary the full block renderer.
  titleElement.innerHTML = inline(escapeHtml(title.text));
  head.append(titleElement);
  if (item.effective !== undefined) {
    const effective = document.createElement("span");
    effective.className = "news-digest-effective";
    effective.setAttribute("data-effective", "");
    effective.textContent = `effective ${item.effective.text}`;
    head.append(effective);
  }
  const summaryElement = document.createElement("div");
  summaryElement.className = "news-digest-summary md";
  summaryElement.innerHTML = renderMarkdown(summary.text);
  const links = document.createElement("div");
  links.className = "news-digest-links";
  for (const link of item.links) links.append(renderExternalLink(ctx, { text: link.label, url: link.url }));
  element.append(head, summaryElement, links);
  return element;
}

/** One source row: its name as a link, and how reading it went. */
export function drawSource(ctx: AppContext, source: NewsDigestSource, path: string): HTMLElement {
  const outcome = requireCase(source.outcome, `${path}.outcome`);
  const element = document.createElement("span");
  element.className = "news-digest-source";
  element.setAttribute("data-outcome", outcome.case);
  const said = document.createElement("span");
  said.className = "news-digest-source-outcome";
  switch (outcome.case) {
    case "read":
      said.textContent = `${outcome.value.newEntries} new`;
      break;
    case "failed":
      said.textContent = `failed: ${outcome.value.reason}`;
      break;
    default: {
      const other: { case: string } = outcome;
      return unreachableArm(`${path}.outcome`, other.case);
    }
  }
  element.append(renderExternalLink(ctx, { text: source.name, url: source.url }), said);
  return element;
}

/** The covered span, in the reader's locale: "Oct 1, 9:00 AM – Oct 2, 9:00 AM". */
export function formatPeriod(fromMs: number, toMs: number): string {
  return new Intl.DateTimeFormat(undefined, { dateStyle: "medium", timeStyle: "short" }).formatRange(
    new Date(fromMs),
    new Date(toMs),
  );
}
