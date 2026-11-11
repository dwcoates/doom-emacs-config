<!-- used by: daemon/internal/gather (a workspace gather's notice to its caller: the first update, or one later answer); placeholders: {{label}}, {{gather}}, {{notice}} -->
{{label}}

This is a notice from a workspace gather you made (`agent-repld workspace gather`), not a request from the user. It carries the gather's `agentrepl.v1.GatherNotice`: `firstUpdate` holds every answer so far (`resolved`, each under its workspace's name, `answered.text` or `closed`) and every target still `outstanding` with why; `laterAnswer` holds one answer that came after the first update and how many targets are still outstanding.

```json
{{notice}}
```

Act on the answers as the work you gathered them for calls for. `agent-repld workspace gather-status {{gather}}` reads the gather again at any time.
