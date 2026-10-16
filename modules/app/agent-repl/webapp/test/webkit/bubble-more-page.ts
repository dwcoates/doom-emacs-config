/**
 * The page module the bubble measurer's WebKit suites bundle: the REAL
 * `refreshHasMore` (src/feed/bubble-more.ts, which places a clamp's `…`
 * through src/feed/ellipsis-trim.ts and fits a collapsed bubble's width
 * through src/feed/collapsed-width.ts), under the webapp's own logger,
 * exposed on the window name each suite bundles it under, beside the clamp's
 * own first-line walk (`domClampGeometry`), which the suites read a clamp's
 * characters through.
 */
import { installPageLogger } from "./page-log";
import { refreshHasMore } from "../../src/feed/bubble-more.js";
import { domClampGeometry } from "../../src/feed/ellipsis-trim.js";

installPageLogger("bubble-more");

export { domClampGeometry, refreshHasMore };
