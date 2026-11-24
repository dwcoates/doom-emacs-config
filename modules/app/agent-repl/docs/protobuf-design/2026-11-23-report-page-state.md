# ReportPageState: Emacs tells the daemon which pages its web view holds

The owner sees about 21 pages loaded for about 50 visible workspaces and wants
the metrics alone to say why each page is loaded. Emacs decides every page load
and unload (`lisp/page-unload.el`), so only Emacs can say it; visibility makes a
page eligible and no longer says whether it is held (owner rulings of
2026-11-23). The daemon needs the fact for `agentrepl_pages_loaded`,
`agentrepl_page_loads_total`, `agentrepl_page_unloads_total`, and the sidebar's
loaded-page count.

Design mode: autonomous.

## Assumptions

- The objective is measurement only: the report changes no page, park or
  visibility behavior.
- The systems involved are Emacs (producer) and the daemon (consumer). The
  webapp and the shim are untouched.
- The change is purely additive: one new rpc in the HOST section.

## Iteration sequence

Transport (ConnectRPC unary, as every HOST verb), then the inventory (one rpc,
`ReportPageState`), then the standing outcome convention (`oneof result`
carried forward with `ReportPageUnloading`'s refusal arms), then the one shape.

## Landed changes

### ReportPageState (endpoint_report_page_state.proto, service.proto HOST)

- WHAT: one unary rpc per workspace. Its request carries the workspace ref and a
  `report` oneof of four arms: `loaded` and `unloaded` (a change that just
  happened, each with its reason), and `held` and `not_held` (a restatement of
  what stands). The load reasons are the five moments `page-unload.el`'s one
  load path takes (`shown`, `awake`, `slot`, `load_requested`, `ungated`); the
  unload reasons are the three ways a page leaves the web view (`dormant`,
  `detached`, `released`). Reasons are oneofs of empty arm messages, never
  enums, per the state-enum rule; the arm name is the metrics label.
- WHY: the counters must count each change once, and the gauge must say what
  stands now, across Emacs restarts and daemon handovers.
- WHY NOT a bool `restated` beside the page fact: it would select the
  interpretation of the adjacent data (counted or not), which the
  boolean-interpretation rule makes a oneof; four arms make "a restated load
  counted" unrepresentable.
- WHY NOT an editor-wide snapshot report (every held page in one call, like
  `ReportLatencies`): the daemon could not tell when an editor's snapshot went
  stale (Emacs exiting leaves no call), so a stale page would stay counted.
  Scoping each workspace's state to its WatchHostWorkspace stream makes a
  stale page structurally impossible: the state is forgotten when the last host
  stream of the workspace ends, and Emacs restates on every host subscribe.
- CONSEQUENCES: Emacs restates on every `elisp.host.subscribed`; a daemon that
  takes a workspace over shows no page for it until that restatement arrives
  (seconds). A failed report is recorded by Emacs and leaves the daemon's view
  stale only until the next report or restatement (accepted cost: fire and
  forget, like `MarkWorkspaceViewed`).
- IMPLEMENTATION: the daemon's handler is `server.ReportPageState`
  (`daemon/internal/server/streams.go`), the state table is
  `runtimemetrics` pages, forgotten in `server.holdParticipant` when the last
  host stream ends. The elisp encoder is in `lisp/wire-host.el`, the sender in
  `lisp/page-report.el`.

Obviated-declaration sweep: nothing; the change only adds.
