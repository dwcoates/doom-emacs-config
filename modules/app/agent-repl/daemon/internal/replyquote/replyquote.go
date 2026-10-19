// Package replyquote is THE QUOTE A REPLY CARRIES: the one place the daemon
// composes the quote of a selected bubble into a prompt, and the one place it
// tells the quote apart from the words the person typed.
//
// A prompt sent while a feed bubble is selected is a reply to that bubble. The
// quote rides the prompt as its own block (conversation.v1.UserQuoteBlock),
// ahead of the blocks the person composed, so the record keeps the person's
// words and the quoted reference apart: the feed draws the quote only in the
// expanded bubble, and nothing downstream has to parse markers out of text.
//
// The block's text is the quote AS DELIVERED: the agent receives it verbatim
// (the shim delivers it as it delivers a text block, blocks joined by a
// newline), and a client draws it verbatim. Composing it once here is what
// keeps the agent's text and the drawn text the same.
package replyquote

import (
	"errors"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// The reply preamble around the quoted text. Kept verbatim: the exact wording
// is the contract with the agent (and asserted character for character by the
// tests), so a change here is a change to what the model is told.
const (
	// HeadingResponse names a quoted response of the agent's.
	HeadingResponse = "⟢ Replying to an earlier response of yours"
	// HeadingPrompt names a quoted PROMPT: it was not the agent's response.
	HeadingPrompt = "⟢ Replying to an earlier prompt in this conversation"
	// ClosingMarker introduces the person's own words.
	ClosingMarker = "⟢ My message"

	// openingResponse introduces a quoted response of the agent's.
	openingResponse = HeadingResponse + ":\n\n"
	// openingPrompt introduces a quoted PROMPT.
	openingPrompt = HeadingPrompt + ":\n\n"
	// closing introduces the person's own words. It ENDS IN A NEWLINE because
	// the blocks of a prompt are delivered joined by one, so the person's words
	// follow it after a blank line.
	closing = "\n\n" + ClosingMarker + ":\n"
)

// minFence is the shortest code fence markdown recognizes.
const minFence = 3

// Fence is the code fence QUOTED is wrapped in: a run of backticks one longer
// than the longest run of backticks anywhere in it, and never shorter than
// three, so no fence or inline code inside the quoted text can close it.
func Fence(quoted string) string {
	longest, run := 0, 0
	for _, r := range quoted {
		if r == '`' {
			run++
			longest = max(longest, run)
			continue
		}
		run = 0
	}
	return strings.Repeat("`", max(minFence, longest+1))
}

// Fenced is TEXT inside its own code fence (Fence), on lines of its own, so
// nothing inside the text can close the fence or be read as the prompt's own
// markdown. Every daemon prompt that carries text it did not write embeds it
// through here.
func Fenced(text string) string {
	fence := Fence(text)
	return fence + "\n" + text + "\n" + fence
}

// Block composes the quote of a selected bubble: the preamble naming what is
// quoted (a PROMPT, or a response of the agent's), MARKDOWN inside its code
// fence, and the marker introducing the person's words.
func Block(markdown string, prompt bool) *conversationv1.UserContentBlock {
	opening := openingResponse
	if prompt {
		opening = openingPrompt
	}
	text := opening + Fenced(markdown) + closing
	return &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Quote{Quote: &conversationv1.UserQuoteBlock{Text: text}},
	}
}

// Parts is a composed quote taken apart again: the heading naming what is
// quoted (HeadingResponse or HeadingPrompt, without its colon), the quoted
// markdown unfenced, and the marker introducing the person's words
// (ClosingMarker, without its colon). A client draws each part in its own
// place, so it never parses the composed text itself.
type Parts struct {
	Heading string
	Quoted  string
	Closing string
}

// ErrNotComposed refuses a text Block did not compose: a quote block's text
// is only ever Block's, so one that does not split is a defect upstream.
var ErrNotComposed = errors.New("the quote text was not composed by replyquote.Block")

// Split is TEXT, a quote Block composed, taken apart into its Parts: the
// inverse of Block, so Split(Block(m, p).text).Quoted is m for every m. TEXT
// that Block could not have composed is refused (ErrNotComposed).
func Split(text string) (Parts, error) {
	var heading, rest string
	switch {
	case strings.HasPrefix(text, openingResponse):
		heading, rest = HeadingResponse, text[len(openingResponse):]
	case strings.HasPrefix(text, openingPrompt):
		heading, rest = HeadingPrompt, text[len(openingPrompt):]
	default:
		return Parts{}, ErrNotComposed
	}
	fencedText, ok := strings.CutSuffix(rest, closing)
	if !ok {
		return Parts{}, ErrNotComposed
	}
	fence, body, ok := strings.Cut(fencedText, "\n")
	if !ok || !strings.HasSuffix(body, "\n"+fence) {
		return Parts{}, ErrNotComposed
	}
	quoted := body[:len(body)-len("\n"+fence)]
	if Fenced(quoted) != fencedText {
		return Parts{}, ErrNotComposed
	}
	return Parts{Heading: heading, Quoted: quoted, Closing: ClosingMarker}, nil
}

// Quote is SAID replying to a selected bubble: the bubble's quote ahead of
// every block the person composed, each kept as it was, in order. SAID itself
// is not modified.
func Quote(said *conversationv1.UserSaid, markdown string, prompt bool) *conversationv1.UserSaid {
	blocks := make([]*conversationv1.UserContentBlock, 0, len(said.GetContent().GetBlocks())+1)
	blocks = append(blocks, Block(markdown, prompt))
	blocks = append(blocks, said.GetContent().GetBlocks()...)
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
}

// isQuote reports whether BLOCK is a quote rather than something the person
// composed.
func isQuote(block *conversationv1.UserContentBlock) bool {
	_, ok := block.GetBlock().(*conversationv1.UserContentBlock_Quote)
	return ok
}

// Words is what the person composed in SAID: every block but the quotes, in
// order. It is what an editor is handed to edit, because the quotes are the
// daemon's and stay as they are through an edit.
func Words(said *conversationv1.UserSaid) *conversationv1.UserSaid {
	blocks := make([]*conversationv1.UserContentBlock, 0, len(said.GetContent().GetBlocks()))
	for _, block := range said.GetContent().GetBlocks() {
		if !isQuote(block) {
			blocks = append(blocks, block)
		}
	}
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
}

// ErrEditedQuote refuses an edit whose content carries a quote. An editor is
// handed the person's words only (Words) and can compose no quote, so one
// arriving in an edit is a defect in the editor, never a quote to keep.
var ErrEditedQuote = errors.New("the edited content carries a quote block, and an editor edits only the person's words")

// Requote is EDITED, the person's edited words, replying to whatever ORIGINAL
// replied to: ORIGINAL's quotes, in order, ahead of the edited blocks. An
// edit changes what the person typed, never what they replied to.
func Requote(original, edited *conversationv1.UserSaid) (*conversationv1.UserSaid, error) {
	for _, block := range edited.GetContent().GetBlocks() {
		if isQuote(block) {
			return nil, ErrEditedQuote
		}
	}
	blocks := make([]*conversationv1.UserContentBlock, 0,
		len(original.GetContent().GetBlocks())+len(edited.GetContent().GetBlocks()))
	for _, block := range original.GetContent().GetBlocks() {
		if isQuote(block) {
			blocks = append(blocks, block)
		}
	}
	blocks = append(blocks, edited.GetContent().GetBlocks()...)
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}, nil
}
