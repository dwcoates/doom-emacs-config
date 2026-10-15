/**
 * THE PAGE THE FEED TEXT ZOOM'S WEBKIT SUITE DRIVES (feed-zoom.webkit.test.ts):
 * the REAL bubble pipeline (`drawBubble`, `markdownSlot`, the body painter and
 * its tree wrapper) under the webapp's own logger, exposed on
 * `window.FeedZoom`. The suite opens the feed shell (feed-page.ts) over the
 * real stylesheet and adds this bundle.
 */
import { markdownSlot } from "../../src/bubble/body.js";
import { drawBubble } from "../../src/bubble/draw.js";
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";

setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    () => undefined,
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-feed-zoom-page" });

/** Draw a response bubble of MARKDOWN as the feed's next row, under ID. */
export function drawResponse(id: string, markdown: string): void {
  const feed = document.getElementById("feed");
  if (feed === null) throw new Error("the page's shell did not mount");
  const row = document.createElement("article");
  row.className = "feed-item";
  row.id = id;
  const { bubble } = drawBubble({
    role: "response",
    variant: "response",
    content: [markdownSlot("prose", markdown)],
    capLines: "feed",
  });
  row.append(bubble);
  feed.append(row);
}
