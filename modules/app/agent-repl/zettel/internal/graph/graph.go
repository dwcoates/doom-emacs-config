package graph

import (
	"context"
	"crypto/rand"
	"database/sql"
	"encoding/hex"
	"errors"
	"fmt"
	"time"

	"agentrepl/zettel/internal/zlog"
)

// ThreadID is a thread's identity: the daemon's workspace id for it.
type ThreadID string

// NodeID is a node's identity, minted here.
type NodeID string

// Quote is the words a question was asked about, and the text around them.
type Quote struct {
	Text string
	// Context is the surrounding text; nil when the view captured none.
	Context *string
}

// AskedOf is a parent that is another node's answer.
type AskedOf struct {
	Node     NodeID
	Revision uint32
	// Quote is nil when the question was asked of the whole answer.
	Quote *Quote
}

// Parent is where a question came from. A nil Of is the thread's start.
type Parent struct {
	Of *AskedOf
}

// Revision is one version of an answer.
type Revision struct {
	Number    uint32
	Markdown  string
	WrittenAt time.Time
}

// Node is one answered question.
type Node struct {
	ID     NodeID
	Thread ThreadID
	// Turn is the daemon turn the node is: recording is idempotent by it.
	Turn     string
	Parent   Parent
	Question string
	// Abbreviation is the question in a few words; nil until one is set.
	Abbreviation *string
	Answer       Revision
	CreatedAt    time.Time
	Sightings    uint64
}

// Refusals: requests the graph turns down without writing anything.
var (
	// ErrUnknownParent: the parent node is not in the thread, or its revision
	// does not exist.
	ErrUnknownParent = errors.New("the parent node or revision does not exist in this thread")
	// ErrThreadAlreadyStarted: the thread already has a root from another
	// turn.
	ErrThreadAlreadyStarted = errors.New("the thread already started")
	// ErrBlankQuery: a search named no word to search for.
	ErrBlankQuery = errors.New("the search names no word")
)

// UnknownNodeError names the first node a request named that does not exist.
type UnknownNodeError struct{ Node NodeID }

func (e *UnknownNodeError) Error() string { return fmt.Sprintf("no node has id %s", e.Node) }

func randomID() string {
	var b [16]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("graph: the system random source failed: %v", err))
	}
	return hex.EncodeToString(b[:])
}

// RecordExchange records one answered question as a node whose first revision
// is the answer, and ABBREVIATION its question's abbreviation when one was
// already minted (nil otherwise). IDEMPOTENT BY TURN: a turn already recorded
// answers its node unchanged, whatever the call carries, so learning one
// turn's answer twice (live, then on a replay) is one node. The bool reports
// whether this call created the node.
func (db *DB) RecordExchange(ctx context.Context, thread ThreadID, turn, question string, abbreviation *string, parent Parent, markdown string) (Node, bool, error) {
	log := db.log.With(zlog.Context{"thread": string(thread), "turn": turn})
	var node Node
	created := false
	err := db.inWrite(ctx, log, "zettel.exchange.record", func(tx *sql.Tx) error {
		existing, err := scanNodeRow(tx.QueryRowContext(ctx, nodeSelect+` WHERE node.turn_id = ?`, turn))
		switch {
		case err == nil:
			node = existing
			return nil
		case !errors.Is(err, sql.ErrNoRows):
			return storagef(err, "looking up the turn's node")
		}
		if parent.Of == nil {
			var roots int
			if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM node WHERE thread_id = ? AND parent_node_id IS NULL`, string(thread)).Scan(&roots); err != nil {
				return storagef(err, "counting the thread's roots")
			}
			if roots > 0 {
				return ErrThreadAlreadyStarted
			}
		} else if err := requireParent(ctx, tx, thread, *parent.Of); err != nil {
			return err
		}
		now := db.now()
		node = Node{
			ID: NodeID(db.newID()), Thread: thread, Turn: turn, Parent: parent, Question: question,
			Abbreviation: abbreviation, Answer: Revision{Number: 1, Markdown: markdown, WrittenAt: now}, CreatedAt: now,
		}
		pn, pr, qt, qc := parentColumns(parent)
		if _, err := tx.ExecContext(ctx, `
			INSERT INTO node (id, thread_id, turn_id, parent_node_id, parent_revision, quote_text, quote_context, question, question_abbreviation, created_at_ms)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			string(node.ID), string(thread), turn, pn, pr, qt, qc, question, abbreviation, now.UnixMilli()); err != nil {
			return storagef(err, "inserting the node")
		}
		if _, err := tx.ExecContext(ctx, `INSERT INTO revision (node_id, number, markdown, written_at_ms) VALUES (?, 1, ?, ?)`,
			string(node.ID), markdown, now.UnixMilli()); err != nil {
			return storagef(err, "inserting the first revision")
		}
		created = true
		return nil
	})
	if err != nil {
		return Node{}, false, err
	}
	if created {
		log.Info("zettel.exchange.record", "an answered question became a node", zlog.Context{"node": string(node.ID), "thread_start": parent.Of == nil})
	} else {
		log.Debug("zettel.exchange.record", "the turn was already recorded; its node is unchanged", zlog.Context{"node": string(node.ID)})
	}
	return node, created, nil
}

func requireParent(ctx context.Context, tx *sql.Tx, thread ThreadID, of AskedOf) error {
	var found int
	err := tx.QueryRowContext(ctx, `
		SELECT COUNT(*) FROM node JOIN revision ON revision.node_id = node.id
		 WHERE node.id = ? AND node.thread_id = ? AND revision.number = ?`,
		string(of.Node), string(thread), of.Revision).Scan(&found)
	if err != nil {
		return storagef(err, "looking up the parent node")
	}
	if found == 0 {
		return ErrUnknownParent
	}
	return nil
}

func parentColumns(p Parent) (node, revision, quoteText, quoteContext any) {
	if p.Of == nil {
		return nil, nil, nil, nil
	}
	node, revision = string(p.Of.Node), p.Of.Revision
	if p.Of.Quote != nil {
		quoteText = p.Of.Quote.Text
		if p.Of.Quote.Context != nil {
			quoteContext = *p.Of.Quote.Context
		}
	}
	return node, revision, quoteText, quoteContext
}

// RecordSightings adds one sighting per occurrence of each node. All or
// nothing.
func (db *DB) RecordSightings(ctx context.Context, nodes []NodeID) error {
	log := db.log.With(zlog.Context{"nodes": len(nodes)})
	return db.inWrite(ctx, log, "zettel.sightings.record", func(tx *sql.Tx) error {
		for _, id := range nodes {
			res, err := tx.ExecContext(ctx, `UPDATE node SET sightings = sightings + 1 WHERE id = ?`, string(id))
			if err != nil {
				return storagef(err, "counting a sighting")
			}
			n, err := res.RowsAffected()
			if err != nil {
				return storagef(err, "confirming a sighting")
			}
			if n == 0 {
				return &UnknownNodeError{Node: id}
			}
		}
		return nil
	})
}

// SetQuestionAbbreviation sets NODE's question abbreviation, replacing any it
// held, and answers the node as it now stands. An unknown node is an
// *UnknownNodeError and changes nothing.
func (db *DB) SetQuestionAbbreviation(ctx context.Context, id NodeID, abbreviation string) (Node, error) {
	log := db.log.With(zlog.Context{"node": string(id)})
	var node Node
	err := db.inWrite(ctx, log, "zettel.abbreviation.set", func(tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE node SET question_abbreviation = ? WHERE id = ?`, abbreviation, string(id))
		if err != nil {
			return storagef(err, "setting the question abbreviation")
		}
		n, err := res.RowsAffected()
		if err != nil {
			return storagef(err, "confirming the question abbreviation")
		}
		if n == 0 {
			return &UnknownNodeError{Node: id}
		}
		node, err = scanNodeRow(tx.QueryRowContext(ctx, nodeSelect+` WHERE node.id = ?`, string(id)))
		if err != nil {
			return storagef(err, "reading the abbreviated node")
		}
		return nil
	})
	if err != nil {
		return Node{}, err
	}
	log.Info("zettel.abbreviation.set", "a node's question was abbreviated", zlog.Context{"abbreviation": abbreviation})
	return node, nil
}

// nodeSelect reads a node with its newest revision.
const nodeSelect = `
		SELECT node.id, node.thread_id, node.turn_id, node.parent_node_id, node.parent_revision, node.quote_text, node.quote_context,
		       node.question, node.question_abbreviation, node.created_at_ms, node.sightings, revision.number, revision.markdown, revision.written_at_ms
		  FROM node
		  JOIN revision ON revision.node_id = node.id
		   AND revision.number = (SELECT MAX(number) FROM revision r WHERE r.node_id = node.id)`

// Thread reads one thread's nodes, oldest first.
func (db *DB) Thread(ctx context.Context, thread ThreadID) ([]Node, error) {
	rows, err := db.read.QueryContext(ctx, nodeSelect+` WHERE node.thread_id = ? ORDER BY node.created_at_ms, node.id`, string(thread))
	if err != nil {
		db.log.Error("zettel.thread.read", "reading the thread's nodes failed", zlog.Context{"thread": string(thread), "cause": err.Error()})
		return nil, storagef(err, "reading the thread's nodes")
	}
	nodes, err := collect(rows, func(r *sql.Rows) (Node, error) { return scanNodeRow(r) })
	if err != nil {
		db.log.Error("zettel.thread.read", "reading the thread's nodes failed", zlog.Context{"thread": string(thread), "cause": err.Error()})
		return nil, storagef(err, "reading the thread's nodes")
	}
	return nodes, nil
}

type scanner interface{ Scan(...any) error }

func scanNodeRow(s scanner) (Node, error) {
	var n Node
	var parentNode, quoteText, quoteContext, abbreviation sql.NullString
	var parentRevision sql.NullInt64
	var created, written int64
	if err := s.Scan(&n.ID, &n.Thread, &n.Turn, &parentNode, &parentRevision, &quoteText, &quoteContext,
		&n.Question, &abbreviation, &created, &n.Sightings, &n.Answer.Number, &n.Answer.Markdown, &written); err != nil {
		return Node{}, err
	}
	n.Parent = parentFromColumns(parentNode, parentRevision, quoteText, quoteContext)
	if abbreviation.Valid {
		a := abbreviation.String
		n.Abbreviation = &a
	}
	n.CreatedAt = time.UnixMilli(created)
	n.Answer.WrittenAt = time.UnixMilli(written)
	return n, nil
}

func parentFromColumns(node sql.NullString, revision sql.NullInt64, quoteText, quoteContext sql.NullString) Parent {
	if !node.Valid {
		return Parent{}
	}
	of := &AskedOf{Node: NodeID(node.String), Revision: uint32(revision.Int64)}
	if quoteText.Valid {
		of.Quote = &Quote{Text: quoteText.String}
		if quoteContext.Valid {
			c := quoteContext.String
			of.Quote.Context = &c
		}
	}
	return Parent{Of: of}
}

func collect[T any](rows *sql.Rows, scan func(*sql.Rows) (T, error)) ([]T, error) {
	defer rows.Close()
	var out []T
	for rows.Next() {
		v, err := scan(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, v)
	}
	return out, rows.Err()
}

// inWrite runs fn in one IMMEDIATE transaction, committing on success. A
// storage failure is logged here, the one place every write passes; a refusal
// is the caller's to log.
func (db *DB) inWrite(ctx context.Context, log *zlog.Logger, op string, fn func(*sql.Tx) error) error {
	tx, err := db.write.BeginTx(ctx, nil)
	if err != nil {
		log.Error(op, "beginning the write failed", zlog.Context{"cause": err.Error()})
		return storagef(err, "beginning the write")
	}
	if err := fn(tx); err != nil {
		if rbErr := tx.Rollback(); rbErr != nil {
			log.Error(op, "rolling back a failed write failed", zlog.Context{"cause": rbErr.Error()})
		}
		if errors.Is(err, ErrStorage) {
			log.Error(op, "the write failed; nothing of it was applied", zlog.Context{"cause": err.Error()})
		}
		return err
	}
	if err := tx.Commit(); err != nil {
		log.Error(op, "committing the write failed", zlog.Context{"cause": err.Error()})
		return storagef(err, "committing")
	}
	return nil
}
