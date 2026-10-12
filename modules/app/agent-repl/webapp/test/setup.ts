import { beforeEach } from "vitest";
import { resetPageState } from "./page-state.js";
import { resetOrderKeys } from "./feed-order.js";
import { installJsdomCapabilities } from "./jsdom-capabilities.js";

/**
 * jsdom performs no layout and runs no animations, so the observers and
 * `Element.getAnimations` the app reaches for are substituted, once per
 * environment (jsdom-capabilities.ts). A missing CAPABILITY of the
 * environment rather than a seam in the app.
 */
installJsdomCapabilities();

/**
 * Every test starts on a fresh page-wide state: production's logger, no
 * standing link verdict, no compaction line. See page-state.ts for each.
 */
beforeEach(resetPageState);

/** Every test mints its fixture rows' order keys afresh (feed-order.ts). */
beforeEach(resetOrderKeys);
