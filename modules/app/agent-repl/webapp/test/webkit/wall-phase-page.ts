/**
 * The page module the rail's wall-phase WebKit suite bundles: the REAL
 * `installWallPhase`, under the webapp's own logger (which the module writes
 * through), exposed on `window.WallPhase`.
 */
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import { installWallPhase } from "../../src/sidebar/wall-phase.js";

setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    () => undefined,
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-wall-phase-page" });

export { installWallPhase };
