<!-- used by: daemon/internal/workspace/answers.go (TellColdGatePassFailed, the notice a workspace's agent is told when the pass of a cold gate its `agent-repld workspace prompt` started failed); placeholders: {{workspace}}, {{why}} -->
The prompt you sent `{{workspace}}` met its cold gate, and agent-repl passed the gate for you by compacting `{{workspace}}`'s whole context with Haiku. That compaction failed, so the gate stands again and your prompt still waits at it, undelivered:

{{why}}

agent-repl does not retry the compaction for this prompt. The user answers the gate on its card, and your prompt is delivered once the session comes up. Notice only; act only if your work depends on that prompt being delivered.
