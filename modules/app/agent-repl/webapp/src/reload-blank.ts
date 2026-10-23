/**
 * reload-blank — HOW LONG A RELOAD LEFT THIS PAGE BLANK IN FRONT OF THE USER.
 *
 * `reload_blank` (proto/vocab/latencies.json) starts in Emacs, when it starts
 * a reload of this page: the host's reload, which reloads every page, or this
 * page's own. Emacs hands the instant to the host with the call that loads the
 * page, and the host hands it here with the presence its hello replays
 * (host/protocol.ts `reloadStartedAtMs`). It ends here, at the page's
 * conversation drawn: the moment the reloaded page stops being blank.
 *
 * ONLY A PAGE SHOWN WHEN IT DRAWS MEASURES. A page held behind the shown one
 * was blank where nobody could see it; a switch to it later is a switch, and
 * switch-latency.ts measures that. Both clocks are the host's wall clock
 * (wall-latency.ts).
 */
import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import type { Presence } from "./presence.js";
import type { SwitchLatencyEnv } from "./switch-latency.js";
import { recordWallLatency, type RecordLatency } from "./wall-latency.js";

/** Measure the reload that loaded the page PRESENCE describes; answers the stop. */
export function watchReloadBlank(presence: Presence, env: SwitchLatencyEnv, record: RecordLatency): () => void {
  return env.onDrawn(() => {
    const startedAtMs = presence.reloadStartedAt();
    if (startedAtMs === null || !presence.shown()) return;
    recordWallLatency(ReportedLatencyKind.RELOAD_BLANK, startedAtMs, env.wallNow(), record);
  });
}
