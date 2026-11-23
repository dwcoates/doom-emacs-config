# Keeping a workspace live

- `workspace keep-live WS` runs the daemon's own keep-alive on WS now, the
  same subroutine its scheduled beats run, and answers when it has ended:
  - a parked session is restored (at most three at once across the daemon),
    sent its keep-alive, the vendor's answer awaited, and stood down again;
  - a session already up is sent its keep-alive and left to the ordinary
    stand-down rules, so one in view or in remote control stays up.
- **Use it whenever you must touch or warm a parked workspace's session, above
  all across many workspaces**, rather than starting its shim some other way
  and leaving it up.
  - Example: before or after a model, effort or permission-mode change across
    many workspaces, keep each live so every shim ends down again.
  - Run one call per workspace; the daemon paces the starts itself.
- It never pays a cold gate and never interrupts work:
  - a cold-gated workspace is refused (`cold_gated`); pass the gate first
    (`workspace pass-gate`) when it should be warmed;
  - a busy session answers `busy` (its turn or detached work keeps the cache
    warm), with nothing sent and nothing stopped.
- Success arms: `beaten` (`started` or `already_up`; its `ack`; `stood_down`
  or `left_up` with the rule that kept it up) or `busy`.
- Refusal arms: `unknown_workspace`, `workspace_ref_mismatch`,
  `transferring_away`, `not_yet_adopted`, `closed`, `cold_gated`,
  `owes_no_keepalive` (with the keeper's reason: hidden, a known-cold cache,
  no session, idle past the cutoff), `cache_lapsed` (the session is parked
  and its next prompt meets the cold gate), `start_failed`, `beat_failed`,
  `stand_down_failed`, `timed_out` (the step, its bound and its measured
  expected time; the daemon recorded it at ERROR), `registry_unreadable`.
