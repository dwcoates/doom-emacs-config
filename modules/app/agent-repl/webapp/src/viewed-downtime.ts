/**
 * viewed-downtime — HOW LONG THE WORKSPACE THE USER IS VIEWING COULD NOT BE
 * USED WHILE A DAEMON CHANGE TOOK ITS DAEMON AWAY.
 *
 * `viewed_downtime` (proto/vocab/latencies.json) starts in Emacs, at the
 * instant it finds the shown workspace's host stream detached or handed on by
 * a daemon change (lisp/webhost.el `agent-repl-webhost-note-downtime`). Emacs
 * hands the instant to the host with the call that loads the page, and the
 * host hands it here with the presence its hello replays (host/protocol.ts
 * `downtimeStartedAtMs`). It ends here, at the page's conversation drawn: a
 * page loads against the serving daemon only once Emacs has walked the
 * workspace onto it, so the drawn page is the moment both the stream and the
 * page are back.
 *
 * ONLY A PAGE SHOWN WHEN IT DRAWS MEASURES, as reload-blank.ts. Both clocks
 * are the host's wall clock (wall-latency.ts).
 *
 * WHICH PART WAS SLOWER is recorded at INFO and not as a metric label: the
 * stream's part is the start to the page's reload (when the reload began
 * after the start), the page's part is the reload to the draw.
 */
import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { log } from "./log.js";
import type { Presence } from "./presence.js";
import type { SwitchLatencyEnv } from "./switch-latency.js";
import { recordWallLatency, type RecordLatency } from "./wall-latency.js";

/** Measure the downtime that preceded the load PRESENCE describes; answers the stop. */
export function watchViewedDowntime(presence: Presence, env: SwitchLatencyEnv, record: RecordLatency): () => void {
  return env.onDrawn(() => {
    const startedAtMs = presence.downtimeStartedAt();
    if (startedAtMs === null || !presence.shown()) return;
    const endedAtMs = env.wallNow();
    const reloadAtMs = presence.reloadStartedAt();
    if (reloadAtMs !== null && reloadAtMs >= startedAtMs && endedAtMs >= reloadAtMs) {
      const streamSeconds = (reloadAtMs - startedAtMs) / 1000;
      const pageSeconds = (endedAtMs - reloadAtMs) / 1000;
      log.info("the viewed workspace's downtime, by part", {
        operation: "page.viewed-downtime-parts",
        context: { streamSeconds, pageSeconds, slower: streamSeconds > pageSeconds ? "stream" : "page" },
      });
    }
    recordWallLatency(ReportedLatencyKind.VIEWED_DOWNTIME, startedAtMs, endedAtMs, record);
  });
}
