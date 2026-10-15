/**
 * The page module the unshown-animations WebKit suite bundles: the REAL pause,
 * its live environment and a real presence, under the webapp's own logger
 * (which the module writes through), exposed on `window.UnshownAnimations`.
 */
import { installPageLogger } from "./page-logger";
import { liveUnshownAnimationsEnv, pauseAnimationsWhileUnshown } from "../../src/host/unshown-animations.js";
import { createPresence } from "../../src/presence.js";

installPageLogger("unshown-animations");

export { createPresence, liveUnshownAnimationsEnv, pauseAnimationsWhileUnshown };
