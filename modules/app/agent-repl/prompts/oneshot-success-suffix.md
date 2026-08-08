<!-- used by: worktree.el (agent-repl--build-oneshot-success-suffix); placeholders: {{invocation}}, {{action_phrase}} -->


When you have successfully implemented the requested change AND written and run the corresponding tests AND committed, invoke {{invocation}} to {{action_phrase}}.

Only invoke {{invocation}} when implementation, tests, and commits are all complete and successful. If you cannot accomplish that — for example, due to genuine prompt ambiguity that you cannot reasonably resolve, or because the implementation cannot be completed — STOP and surface the situation to the user instead of pushing on with a faulty implementation. You have artistic license to resolve minor ambiguity by making best-guess judgments, but if there is genuine ambiguity that materially affects the implementation, prefer to stop and surface it.
