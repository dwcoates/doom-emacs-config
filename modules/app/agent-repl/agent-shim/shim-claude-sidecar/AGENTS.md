# agent-shim/shim-claude-sidecar/

The Claude file-plane reader (Go, singleton, launchd-managed). Responsibility:
observe the Claude harness's on-disk artifacts (session transcripts, agent
sidechain transcripts, workflow journals, `/tmp` task spools), parse them with
cursored, truncation-aware tailing, convert records into agent-shim protocol
events (same loud-validation contract as the shims), infer terminal `LOST`
transitions per the staleness policy, and write everything to the shim-store
with atomic cursor advancement.

The sidecar is 100% specific to Claude's file formats BY DESIGN; its entire job
is converting that vendor reality into the (treated-as-)vendor-agnostic
protocol. It interprets no resolved state and owns no database.

Dependencies: `proto/agentshim/` (generated Go), the shim-store UDS socket,
the Claude harness file formats it parses.
