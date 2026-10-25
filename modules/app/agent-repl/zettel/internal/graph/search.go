package graph

import (
	"context"
	"database/sql"
	"strings"

	"agentrepl/zettel/internal/zlog"
)

// THE SEARCH INDEX is node_text (layoutTwoDDL): every node's question and its
// newest answer. A query's words are each a word or the start of one, all
// required, in any order, case- and accent-insensitively; nothing typed is
// ever read as search syntax (matchExpression).

// SnippetPart is one run of a snippet: words around the match, or words the
// query matched.
type SnippetPart struct {
	Text string
	Hit  bool
}

// Match is one thread that matched, through its best-matching node.
type Match struct {
	Thread  ThreadID
	Node    NodeID
	Snippet []SnippetPart
}

// The marks snippet() wraps a matched run in. Neither can be typed into a
// question or appear in an answer the vendor wrote as text.
const (
	hitOpen  = "\x02"
	hitClose = "\x03"
)

// snippetTokens is how many tokens of context snippet() keeps around a match.
const snippetTokens = 12

// matchExpression turns typed words into an FTS5 expression that is only
// words: each word double-quoted (a quote inside doubled) and prefix-matched,
// the words implicitly ANDed. It answers false for a query with no word.
func matchExpression(query string) (string, bool) {
	words := strings.Fields(query)
	if len(words) == 0 {
		return "", false
	}
	quoted := make([]string, len(words))
	for i, w := range words {
		quoted[i] = `"` + strings.ReplaceAll(w, `"`, `""`) + `"*`
	}
	return strings.Join(quoted, " "), true
}

// Search answers every thread whose nodes say QUERY's words, each once by its
// best-matching node, the best match first. A query with no word is the
// caller's to refuse (ErrBlankQuery).
func (db *DB) Search(ctx context.Context, query string) ([]Match, error) {
	expr, ok := matchExpression(query)
	if !ok {
		return nil, ErrBlankQuery
	}
	rows, err := db.read.QueryContext(ctx, `
		SELECT thread_id, node_id, snippet(node_text, -1, ?, ?, '…', ?)
		  FROM node_text
		 WHERE node_text MATCH ?
		 ORDER BY bm25(node_text), node_id`, hitOpen, hitClose, snippetTokens, expr)
	if err != nil {
		db.log.Error("zettel.search", "searching the graph failed", zlog.Context{"query": query, "cause": err.Error()})
		return nil, storagef(err, "searching the graph")
	}
	all, err := collect(rows, func(r *sql.Rows) (Match, error) {
		var m Match
		var snippet string
		if err := r.Scan(&m.Thread, &m.Node, &snippet); err != nil {
			return Match{}, err
		}
		m.Snippet = snippetParts(snippet)
		return m, nil
	})
	if err != nil {
		db.log.Error("zettel.search", "reading the search's matches failed", zlog.Context{"query": query, "cause": err.Error()})
		return nil, storagef(err, "reading the search's matches")
	}
	seen := map[ThreadID]bool{}
	var best []Match
	for _, m := range all {
		if !seen[m.Thread] {
			seen[m.Thread] = true
			best = append(best, m)
		}
	}
	db.log.Debug("zettel.search", "searched the graph", zlog.Context{"query": query, "nodes": len(all), "threads": len(best)})
	return best, nil
}

// snippetParts splits snippet()'s marked text into its runs.
func snippetParts(marked string) []SnippetPart {
	var parts []SnippetPart
	add := func(text string, hit bool) {
		if text != "" {
			parts = append(parts, SnippetPart{Text: text, Hit: hit})
		}
	}
	for marked != "" {
		open := strings.Index(marked, hitOpen)
		if open < 0 {
			add(marked, false)
			break
		}
		add(marked[:open], false)
		rest := marked[open+len(hitOpen):]
		end := strings.Index(rest, hitClose)
		if end < 0 {
			panic("graph: snippet() opened a match it never closed: " + marked)
		}
		add(rest[:end], true)
		marked = rest[end+len(hitClose):]
	}
	return parts
}

// DeleteThread destroys every node of THREAD with its revisions (the search
// index follows by trigger), in one transaction. A thread with no node
// deletes to nothing. It answers how many nodes were deleted.
func (db *DB) DeleteThread(ctx context.Context, thread ThreadID) (int, error) {
	log := db.log.With(zlog.Context{"thread": string(thread)})
	deleted := 0
	err := db.inWrite(ctx, log, "zettel.thread.delete", func(tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `DELETE FROM revision WHERE node_id IN (SELECT id FROM node WHERE thread_id = ?)`, string(thread)); err != nil {
			return storagef(err, "deleting the thread's revisions")
		}
		res, err := tx.ExecContext(ctx, `DELETE FROM node WHERE thread_id = ?`, string(thread))
		if err != nil {
			return storagef(err, "deleting the thread's nodes")
		}
		n, err := res.RowsAffected()
		if err != nil {
			return storagef(err, "counting the deleted nodes")
		}
		deleted = int(n)
		return nil
	})
	if err != nil {
		return 0, err
	}
	log.Info("zettel.thread.delete", "the thread's graph was deleted", zlog.Context{"nodes_deleted": deleted})
	return deleted, nil
}
