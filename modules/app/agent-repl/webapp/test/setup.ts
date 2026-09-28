import { beforeEach } from "vitest";
import { resetPageState } from "./page-state.js";
import { resetOrderKeys } from "./feed-order.js";
import { installResizeObserver } from "./resize-observer.js";
import { installIntersectionObserver } from "./intersection-observer.js";

/**
 * jsdom implements no `ResizeObserver` (it performs no layout), and the feed
 * mount subscribes one to its scroll box so a footer settling after a render
 * cannot leave the tail below the fold. Installed here, once per environment,
 * because it is a missing CAPABILITY of the environment rather than a seam in
 * the app.
 */
installResizeObserver();

/**
 * jsdom implements no `IntersectionObserver` either (it performs no layout), and
 * the feed mount subscribes one to its scroll box for the overscan pre-render
 * band (overscan.ts). Installed here, once per environment, for the same reason
 * as the `ResizeObserver` stub above — a missing CAPABILITY of the environment,
 * not a seam in the app. Without it every feed mount would take the "no
 * IntersectionObserver" no-op path and the overscan wiring would never run.
 */
installIntersectionObserver();

/**
 * Every test starts on a fresh page-wide state: production's logger, no
 * standing link verdict, no compaction line. See page-state.ts for each.
 */
beforeEach(resetPageState);

/** Every test mints its fixture rows' order keys afresh (feed-order.ts). */
beforeEach(resetOrderKeys);
