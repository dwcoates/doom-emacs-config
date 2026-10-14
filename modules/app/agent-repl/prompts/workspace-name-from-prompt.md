<!-- used by: daemon/internal/workspace (Create, the naming call); placeholders: {{prompt}}, {{conversation}} -->
Name the workspace a developer is about to open for the work described below.

Answer with the NAME AND NOTHING ELSE. No sentence around it, no quotes, no
backticks, no explanation, no trailing punctuation. The whole of your reply is
the name.

THE NAME IS AT MOST 4 WORDS. Count the words before you answer: one, two,
three or four words joined by hyphens, never five or more.
"agent-repl-input-shorter" is four words and is fine;
"agent-repl-input-box-shorter" is five words and is REFUSED. Drop words until
four or fewer remain.

The name must be:

- AT MOST 4 WORDS (one, two, three or four; five or more is refused);
- lowercase;
- hyphen-separated, with no leading or trailing hyphen;
- made only of the letters a-z, the digits 0-9 and those hyphens;
- at most 40 characters in total.

Name the SUBJECT of the work, never its provenance. So: never a chat-channel
or thread name, never a person's name, never a date, a timestamp, a ticket id
or a hash. "flaky-login-test" is a good name; "jane-slack-thread" is not.

Do not add any prefix of your own. The system prefixes the name itself.

A FORKED workspace continues an earlier conversation, which is summarized after
the work. Name what the fork carries on with: the new request decides it when
there is one, and when the new request is empty the earlier conversation's
subject IS the work. Never answer that you need more to go on; the material
below is all there is, and a name is always owed.

The work (empty when a fork carries on its conversation as it stands):

{{prompt}}

The earlier conversation this workspace continues (empty when it starts fresh):

{{conversation}}
