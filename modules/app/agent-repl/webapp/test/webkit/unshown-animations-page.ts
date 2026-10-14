/**
 * The page module the unshown-animations WebKit suite bundles: the REAL pause,
 * its live environment and a real presence, under the webapp's own logger
 * (which the module writes through), exposed on `window.UnshownAnimations`.
 */
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import { liveUnshownAnimationsEnv, pauseAnimationsWhileUnshown } from "../../src/host/unshown-animations.js";
import { createPresence } from "../../src/presence.js";

setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    () => undefined,
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-unshown-animations-page" });

export { createPresence, liveUnshownAnimationsEnv, pauseAnimationsWhileUnshown };
