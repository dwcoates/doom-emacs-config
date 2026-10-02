# Startup bring-up and fault domains

## Owner requirements (2026-10-02)

1. On Emacs startup EVERY workspace is brought up without the user switching
   to it: daemon built/started/linked, every workspace's session woken
   (including hibernated ones), its webview created and its page drawn.
2. A workspace's tab is NOT opened until agent-repl's own services serve it
   (daemon, shim, store, sidecar; plus its page drawn) — the vendor/SDK and the
   network never gate opening. A workspace whose service-level bring-up FAILS
   still opens (showing the failure) as long as the daemon is available.
3. Tabs open strictly in the daemon registry's order: workspace 3 never opens
   before 1 and 2, even if it is ready first.
4. Every step a user cares about is one minibuffer line (and *Messages*):
   intermediate steps end in "…", final ones in ".". The daemon-side steps
   stream over WatchDaemon. List (owner-accepted):
   - agent-repl: building the daemon… / daemon built. / starting the daemon…
     / found a running daemon. / connecting to the daemon… / connected to the
     daemon. / opening N workspaces…
   - agent-repl: WS: starting session… / waking from sleep… / resuming the
     conversation… / Claude did not start, retrying (attempt N)… / loading
     the page… / ready, waiting for AHEAD to open first… / ready.
   - agent-repl: WS: Claude refused to start: CAUSE. / Claude failed to start
     after 10 minutes. / needs your answer to resume (large context). /
     offline, waiting for the network… / session failed to start: REASON.
   - agent-repl: all N workspaces ready. / agent-repl: K of N workspaces
     ready; WS failed to start.
5. Status by FAULT DOMAIN, precedence agent-repl > network > vendor:
   - agent_repl_fault — BLUE, composer closed: agent-repl's own services
     (starting, severed, dead, start_failed, daemon_impaired).
   - network_fault — BLUE: this machine is offline; the shim tells an
     unreachable network apart from a vendor answer.
   - vendor_fault — TURQUOISE, composer OPEN, prompts held "after
     reconnect": everything vendor-specific (auth, usage limit, billing,
     vendor error, api retrying, query died, vendor start retry / rejection /
     failed).
6. Terminology to codify in agent-repl AGENTS.md: "WatchDaemon", "daemon
   watching in Emacs" and "the editor stream" all name Emacs's WatchDaemon
   subscription (WatchDaemonRequest.client = emacs).

## Landed contract (this branch)

- frontend.v1 FooterStatus: `blocked` (7) RENAMED `vendor_fault`
  (FooterStatusVendorFault*), gaining the three vendor-start substatuses
  (9–11) and the `vendor_start` salient line (10), losing `daemon_impaired`
  (reserved 7). `disconnected` (9) RENAMED `agent_repl_fault`
  (FooterStatusAgentReplFault*), gaining `daemon_impaired` (10), losing the
  vendor-start substatuses (reserved 7–9) and `vendor_start` salient (reserved
  8). NEW `network_fault` (17) = FooterStatusNetworkFault {offline} with its
  own activity/salient (`FooterStatusActivityNetworkOffline`). Fault partition
  comment rewritten.
- frontend.v1 RosterRow: NEW `vendor_fault` (50, turquoise) for vendor-start
  retry/rejection/failed; NEW `network_fault` (51, blue); `vendor_blocked`
  (12) is now turquoise.
- conversation.v1 SessionFault: NEW `network_unreachable` (8), opened by the
  shim on a below-the-vendor network failure and resolved on the next request
  that reaches the vendor.
- shim.v1 StartSessionVendorStartRetryable: NEW `oneof cause { network;
  vendor; }`.
- agentrepl.v1 WatchDaemonResponse: NEW `startup` (10) = DaemonStartupEvent
  {opening | workspace_step | workspace_open | finished}, Emacs streams only,
  events not replayed. `workspace_open` is the per-workspace go-ahead in
  registry order, gated on service-level availability (or settled failure).

## Decisions

- Startup events are not replayed; an Emacs that reconnects mid-startup
  reads current state from the roster and host streams.
- The editor opens a tab only after BOTH the daemon's go-ahead and its own
  page drawn, preserving go-ahead order.
- A service-level failure settles at the shim's start bound (the daemon's
  existing StartSession / spawn bounds), then gets its go-ahead.
