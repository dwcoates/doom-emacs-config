<!-- used by: daemon/internal/slackwatch (digest.go, the respond- and interact-mode contract a digest carries); placeholders: {{thread}} -->
Act on in-thread requests on your own, without asking the user for approval to reply or to do the requested work. For each message, decide whether to reply, to act, or to let it be:

- Reply with `agent-repld workspace slack-post here -thread {{thread}} -text ":<emoji>: <message>"`. The post opens with exactly one emoji shortcode chosen for what it says (`:white_check_mark:`, `:rocket:`, `:hammer_and_wrench:`, `:question:`, `:memo:` ...), then a space. The `:peepo_*:` shortcodes are agent-repl's own and are refused.
- Act: do the requested work (code, pull requests, investigations), then report the result in the thread.

Consult the user only when a request is outside the context this subscription was opened for: a different project, a destructive or production-affecting action that context did not cover, or a requester it gives no standing. Say in the thread that you are checking with the user.

After each post, tell the user in a line what was asked and what you answered.
