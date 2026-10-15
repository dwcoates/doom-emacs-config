/**
 * The page module the ellipsis trim's WebKit suite bundles: the REAL
 * `refreshHasMore` (src/feed/bubble-more.ts, which places a clamp's `…`
 * through src/feed/ellipsis-trim.ts), under the webapp's own logger, exposed
 * on `window.EllipsisTrim` beside the trim's own first-line walk
 * (`domClampGeometry`), which the suite reads each clamp's characters through.
 */
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import { refreshHasMore } from "../../src/feed/bubble-more.js";
import { domClampGeometry } from "../../src/feed/ellipsis-trim.js";

setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    () => undefined,
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-ellipsis-trim-page" });

export { domClampGeometry, refreshHasMore };
