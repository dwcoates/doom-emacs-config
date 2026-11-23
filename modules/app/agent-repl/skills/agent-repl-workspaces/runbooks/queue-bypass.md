# Queue bypass mode

- **QUEUE BYPASS MODE.** `workspace set WS -queue-bypass on|off` takes or
  releases WS's repository's queue bypass mode: WS switches to its bypass
  conversation, which lands each change on the main branch itself, pausing
  the repository's merge queue only while it lands one. The queue keeps
  running otherwise.
  - `-context` names, with `on` only, how the bypass session gets its
    context:
    - `full`: a full fork of the queue conversation, replacing any parked
      bypass conversation.
    - `compacted`: a compacted fork of the queue conversation, replacing any
      parked bypass conversation.
    - `existing`: the parked bypass conversation, resumed as it was left.
    - `existing-plus-compacted`: the parked bypass conversation, opening the
      next prompt with a summary of the queue conversation.
  - The two existing options are refused `no_bypass_conversation` while
    nothing is parked, and nothing changes. An `on` naming no context resumes
    the parked bypass conversation, or forks the queue conversation when
    none is parked.
  - A failed compaction or summary is refused `compaction_failed`: the
    switch stands, and the next start of the session asks for it again.
  - Take or release it only when the user asks for it.
  - The design is `modules/app/agent-repl/docs/protobuf-design/queue-bypass-mode.md`.
