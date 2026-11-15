package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// eachDrawnMarkdown replaces every drawn markdown of PARSED with what EDIT
// answers for it: each plain block's, each entry answer's, and each tree
// node's when TREES says to. ENDS tells EDIT the markdown is where the whole
// text ends, the one place a text still arriving can grow: the last block's
// plain markdown or its last tree node's. An entry answer never ends it.
func eachDrawnMarkdown(parsed *frontendv1.ParsedProse, trees bool, edit func(markdown string, ends bool) string) {
	blocks := parsed.GetBlocks()
	for i, block := range blocks {
		last := i == len(blocks)-1
		switch b := block.GetBlock().(type) {
		case *frontendv1.ProseBlock_Plain:
			b.Plain.Markdown = edit(b.Plain.GetMarkdown(), last)
		case *frontendv1.ProseBlock_Tree:
			if !trees {
				continue
			}
			nodes := b.Tree.GetNodes()
			for j, node := range nodes {
				if text := node.GetText(); text != nil {
					text.Markdown = edit(text.GetMarkdown(), last && j == len(nodes)-1)
				}
			}
		case *frontendv1.ProseBlock_Entries:
			for _, node := range b.Entries.GetNodes() {
				if text := node.GetAnswer().GetText(); text != nil {
					text.Markdown = edit(text.GetMarkdown(), false)
				}
			}
		}
	}
}
