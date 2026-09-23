/**
 * artifact — the published page's bubble: purple, response-styled.
 *
 * ONLY A PUBLISH DRAWS. Listing artifacts, reading one, uploading an asset —
 * none of those produce a row, so there is no "browsed" state here and no arm
 * for one. A redeploy of the same page UPSERTS this same bubble rather than
 * adding a second: one page, one row, whatever it took to get there.
 *
 * THE HEADING IS THE DAEMON'S, VERBATIM — favicon emoji included ("📊 Merge
 * Queue Report"). The composition (and the fall back to the filename when the
 * publish named no title) happened daemon-side, so this end neither assembles
 * the line nor adds a glyph of its own to it. The GLYPHS-NOT-EMOJIS directive
 * governs the chrome THIS renderer invents; it cannot govern a string the
 * daemon resolved, and stripping the emoji out of a served heading would be
 * editing the view rather than drawing it.
 *
 * THE URL IS A LINK THROUGH THE SHARED COMPONENT. A real navigation would take
 * the webview away from the conversation, so `renderExternalLink` cancels the
 * click and raises `OpenExternal` instead; a refusal draws at the link.
 *
 * `failed` PUTS THE REASON WHERE THE URL WOULD BE, because that is the line the
 * reader is looking at for the answer to "where is my page".
 */
import type {
  FeedArtifact,
  FeedArtifactFailed,
  FeedArtifactHeading,
  FeedArtifactPublished,
  FeedArtifactUrl,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { renderExternalLink } from "../../link.js";
import { log } from "../../log.js";
import type { AppContext } from "../../rpc/context.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { agenticBubble } from "./controls.js";

const PATH = "FeedArtifact";

/** The badge each state wears, and the word it says. */
const STATE_BADGES = {
  publishing: { className: "badge run", text: "publishing" },
  published: { className: "badge ok", text: "published" },
  failed: { className: "badge err", text: "failed" },
} as const satisfies Record<string, { className: string; text: string }>;

/** Every state this build draws, for the suite to hold to the schema. */
export const ARTIFACT_STATE_ARMS: readonly string[] = Object.keys(STATE_BADGES);

/** The artifact bubble. */
export function drawFeedArtifact(u: FeedArtifact, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  log.debug("drawing an artifact bubble", {
    operation: "feed.cards.artifact",
    context: { state: state.case },
  });

  const heading = drawFeedArtifactHeading(
    requireMessage(u.heading, `${PATH}.heading`),
    `${PATH}.heading`,
  );
  // THE ARM IS CHECKED BEFORE ANYTHING IS DRAWN FROM IT. Badging first would
  // read the arm's own name out of a table an unknown arm is not in, and the
  // refusal that owes the reader the arm's NAME would become a TypeError.
  switch (state.case) {
    case "publishing":
      // The badge IS the state; there is no URL to draw yet, and an empty line
      // where one will appear would read as a page published to nowhere.
      return agenticBubble({ state: state.case, heading, content: [badge(state.case)] });
    case "published":
      return agenticBubble({
        state: state.case,
        heading,
        content: [badge(state.case), drawFeedArtifactPublished(state.value, rc.ctx, `${PATH}.published`)],
      });
    case "failed":
      return agenticBubble({
        state: state.case,
        heading,
        content: [badge(state.case), drawFeedArtifactFailed(state.value, `${PATH}.failed`)],
      });
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
}

/** The composed heading, drawn verbatim. */
export function drawFeedArtifactHeading(u: FeedArtifactHeading, path: string): string {
  log.debug("reading an artifact heading", {
    operation: "feed.cards.artifact.heading",
    context: { path },
  });
  return u.text;
}

/** The published state: the page's URL, linked. */
export function drawFeedArtifactPublished(
  u: FeedArtifactPublished,
  ctx: AppContext,
  path: string,
): HTMLElement {
  log.debug("drawing a published artifact", {
    operation: "feed.cards.artifact.published",
    context: { path },
  });
  const line = document.createElement("div");
  line.className = "artifact-url";
  line.append(drawFeedArtifactUrl(requireMessage(u.url, `${path}.url`), ctx, `${path}.url`));
  return line;
}

/** The URL element: drawn verbatim, and clickable through the one component. */
export function drawFeedArtifactUrl(
  u: FeedArtifactUrl,
  ctx: AppContext,
  path: string,
): HTMLElement {
  log.debug("drawing an artifact url", {
    operation: "feed.cards.artifact.url",
    context: { path, url: u.url },
  });
  return renderExternalLink(ctx, { text: u.url, url: u.url });
}

/** The failed state's composed reason, where the URL would have been. */
export function drawFeedArtifactFailed(u: FeedArtifactFailed, path: string): HTMLElement {
  log.debug("drawing a failed artifact publish", {
    operation: "feed.cards.artifact.failed",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "artifact-failed";
  el.textContent = u.text;
  return el;
}

/** One of the three badges, by arm. */
function badge(arm: keyof typeof STATE_BADGES): HTMLElement {
  const spec = STATE_BADGES[arm];
  const el = document.createElement("span");
  el.className = spec.className;
  el.textContent = spec.text;
  return el;
}
