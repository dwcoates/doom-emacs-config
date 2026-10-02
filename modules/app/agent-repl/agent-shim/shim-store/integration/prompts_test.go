// prompts_test.go — SUBJECT: a delivered prompt is a page line.
//
// StoreAgentItem has exactly two arms and the prompt is one of them: a book that
// showed only the agent's own frames would draw an agent answering questions
// nobody asked. The book is read from the PROMPT ITSELF (AgentPrompt.agent — its
// one recipient), never restated on the envelope.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"testing"
)

func TestADeliveredPromptIsAPageLineOfItsAddresseesBook(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act
	shim.write(ctx, t, shim.agentEntry("w-p-1", "turn-1",
		promptLine(agentID("main"), promptFact("turn-1", "main", "do the thing"))))

	// Assert
	page := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the addressee's book", pageTexts(page.GetPage()), []string{"prompt:do the thing"})
	store.assertNoErrorRecords()
}

func TestAPromptAndTheAnswerItProvokedSharePageOrder(t *testing.T) {
	// Arrange: the prompt comes first in first-insert order, so a page reads as
	// the exchange it was.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act
	shim.write(ctx, t,
		shim.agentEntry("w-p-1", "turn-1", promptLine(agentID("main"), promptFact("turn-1", "main", "ask"))),
		shim.agentEntry("w-p-2", "act-1", frameLine(agentID("main"), responseFrame("main", "act-1", "answer"))),
	)

	// Assert: newest first.
	page := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the exchange", pageTexts(page.GetPage()), []string{"answer", "prompt:ask"})
	store.assertNoErrorRecords()
}

func TestAPromptToASubagentLandsInTheSubagentsOwnBook(t *testing.T) {
	// Arrange: the book is the prompt's recipient, so a prompt delivered to a
	// subagent is never a line of its spawner's book.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act
	shim.write(ctx, t,
		shim.agentEntry("w-p-main", "turn-main", promptLine(agentID("main"), promptFact("turn-main", "main", "to main"))),
		shim.agentEntry("w-p-sub", "turn-sub", promptLine(agentID("main"), promptFact("turn-sub", "sub-1", "to the subagent"))),
	)

	// Assert
	mainBook := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the main agent's book", pageTexts(mainBook.GetPage()), []string{"prompt:to main"})
	subBook := openSession(ctx, t, cli, "sub-1", nil)
	assertTexts(t, "the subagent's book", pageTexts(subBook.GetPage()), []string{"prompt:to the subagent"})
	store.assertNoErrorRecords()
}

func TestAPromptIsStreamedToAStandingWatcher(t *testing.T) {
	// Arrange: a watcher of a book sees the prompts delivered to it, not only
	// the agent's own frames.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	seedBook(ctx, t, shim, "main", "prompt-tail")
	opened := openSession(ctx, t, cli, "main", nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)

	// Act
	shim.write(ctx, t, shim.agentEntry("w-p-tail", "turn-tail",
		promptLine(agentID("main"), promptFact("turn-tail", "main", "tailed prompt"))))

	// Assert
	assertTexts(t, "the tail", receivedTexts(receiveLines(t, stream, 1)), []string{"prompt:tailed prompt"})
	store.assertNoErrorRecords()
}

func TestAPromptSurvivesARestart(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-p-durable", "turn-durable",
		promptLine(agentID("main"), promptFact("turn-durable", "main", "durable prompt"))))

	// Act
	store.restart()

	// Assert
	after, cancelAfter := callContext(t)
	defer cancelAfter()
	page := openSession(after, t, store.client(), "main", nil)
	assertTexts(t, "the book after a restart", pageTexts(page.GetPage()), []string{"prompt:durable prompt"})
	store.assertNoErrorRecords()
}
