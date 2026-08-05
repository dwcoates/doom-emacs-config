# Read-only token-model audit output

- Run at: `2026-08-05T15:09:34-04:00`
- Command: `go run ./cmd/token-utilization-audit --db /Users/dodgecoates/.cache/agent-repl/ssm/state.db`
- Mode: read-only SQLite (`mode=ro`, `query_only`)
- Outcome: `candidate_count=6`, `mutated_count=0`

| Agent REPL session | Claude session | API message | Root turn | Raw model |
| --- | --- | --- | --- | --- |
| `s_4a1c95d44ee24cf1` | `148e5bf4-598d-44bb-9a26-9926c6861eac` | `2a7fedd0-5f09-407e-b6c0-525fef5d5e1f` | `daemon-prompt-2-cd04c0b52676` | `""` |
| `s_5f79f274725475a8` | `3b91fc75-5b1b-4238-81db-55f740a2243f` | `78e24be0-31b1-4ce9-8eeb-401d9e55ffc6` | `daemon-prompt-3-0f9f4ffd2d23` | `""` |
| `s_5f79f274725475a8` | `3b91fc75-5b1b-4238-81db-55f740a2243f` | `d87a65e1-37c6-4ac8-91b6-7df529a0e77f` | `daemon-prompt-2-61878b656b79` | `""` |
| `s_7d1e94a17392b33a` | `e1380737-69eb-4bfe-a7e7-942d160e6d87` | `110f0e34-a65b-4fd1-89bb-8de09264c6a5` | `fe-946-fa3f` | `""` |
| `s_7d1e94a17392b33a` | `e1380737-69eb-4bfe-a7e7-942d160e6d87` | `1a8f5f0b-7e6c-426b-8511-bec864d7d88d` | `fe-952-2611` | `""` |
| `s_a8cb196cf963adc9` | `7258722e-9f3f-4e3e-84ce-64d65853ce12` | `b325ec7d-b379-4738-9b03-26f3924ac7eb` | `daemon-prompt-2-725984b3ca12` | `""` |

No quarantine or delete action was approved or run.
