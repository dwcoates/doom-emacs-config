<!-- used by: daemon/internal/workspace/homequestion.go (AskHomeMain, the question a feed card's ? control sends the home repository's main workspace); placeholders: {{asker}}, {{question}} -->
I pressed the `?` control on a card in the workspace `{{asker}}`, and asked the following about it:

{{question}}

First, acknowledge `{{asker}}` at once with an agent-to-agent message (`agent-repld workspace prompt {{asker}} <text>`), saying in a sentence what you are going to do with this.

Then:

- if what I wrote is a question, answer it yourself, here;
- if it asks for a change, dispatch it as an async one-shot child workspace that lands into the main branch, as your system prompt's notice says, and report the child you dispatched by name.
