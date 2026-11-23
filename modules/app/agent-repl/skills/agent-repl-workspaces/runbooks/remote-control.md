# Remote control

- **REMOTE CONTROL.** `workspace set WS -remote-control on|off` turns WS's
  remote control on or off: its session becomes reachable, or stops being
  reachable, from claude.ai and its mobile app. WS may be this workspace or
  another, named as every verb names one.
  - Change it only when the user asks for it, since it exposes a session to
    their phone and keeps its shim up.
  - A workspace holding its repository's queue bypass mode is in remote
    control: the daemon turns it on at the take and off at the release
    (unless it was on before). NEVER turn it on or off to follow bypass
    mode yourself.
  - How the mode behaves is
    `modules/app/agent-repl/docs/developer/remote-control.md`.
