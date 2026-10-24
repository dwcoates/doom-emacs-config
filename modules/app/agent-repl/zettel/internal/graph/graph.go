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

// ExchangeID is an exchange's identity, minted here.
type ExchangeID string

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
	ID        NodeID
	Thread    ThreadID
	Parent    Parent
	Question  string
	Answer    Revision
	CreatedAt time.Time
	Sightings uint64
}

// Pending is an exchange in flight.
type Pending struct {
	ID       ExchangeID
	Thread   ThreadID
	Turn     string
	Parent   Parent
	Question string
	BegunAt  time.Time
}

// Refusals: requests the graph turns down without writing anything.
var (
	// ErrUnknownParent: the parent node is not in the thread, or its revision
	// does not exist.
	ErrUnknownParent = errors.New("the parent node or revision does not exist in this thread")
	// ErrThreadAlreadyStarted: the thread already has a root, answered or in
	// flight.
	ErrThreadAlreadyStarted = errors.New("the thread already started")
	// ErrUnknownExchange: no exchange in flight has the id.
	ErrUnknownExchange = errors.New("no exchange in flight has this id")
)

// TurnAlreadyBegunError refuses a second exchange for one turn.
type TurnAlreadyBegunError struct{ Existing ExchangeID }

func (e *TurnAlreadyBegunError) Error() string {
	return fmt.Sprintf("an exchange was already begun for this turn: %s", e.Existing)
}

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

// BeginExchange records a question the moment it is sent.
func (db *DB) BeginExchange(ctx context.Context, thread ThreadID, turn, question string, parent Parent) (ExchangeID, error) {
	log := db.log.With(zlog.Context{"thread": string(thread), "turn": turn})
	id := ExchangeID(db.newID())
	err := db.inWrite(ctx, log, "zettel.exchange.begin", func(tx *sql.Tx) error {
		var existing string
		switch err := tx.QueryRowContext(ctx, `SELECT id FROM pending_exchange WHERE turn_id = ?`, turn).Scan(&existing); {
		case err == nil:
			return &TurnAlreadyBegunError{Existing: ExchangeID(existing)}
		case !errors.Is(err, sql.ErrNoRows):
			return storagef(err, "looking up the turn's exchange")
		}
		if parent.Of == nil {
			var roots int
			if err := tx.QueryRowContext(ctx, `
				SELECT (SELECT COUNT(*) FROM node WHERE thread_id = ? AND parent_node_id IS NULL)
				     + (SELECT COUNT(*) FROM pending_exchange WHERE thread_id = ? AND parent_node_id IS NULL)`,
				string(thread), string(thread)).Scan(&roots); err != nil {
				return storagef(err, "counting the thread's roots")
			}
			if roots > 0 {
				return ErrThreadAlreadyStarted
			}
		} else if err := requireParent(ctx, tx, thread, *parent.Of); err != nil {
			return err
		}
		pn, pr, qt, qc := parentColumns(parent)
		if _, err := tx.ExecContext(ctx, `
			INSERT INTO pending_exchange (id, thread_id, turn_id, parent_node_id, parent_revision, quote_text, quote_context, question, begun_at_ms)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			string(id), string(thread), turn, pn, pr, qt, qc, question, db.now().UnixMilli()); err != nil {
			return storagef(err, "inserting the exchange")
		}
		return nil
	})
	if err != nil {
		return "", err
	}
	log.Info("zettel.exchange.begin", "an exchange began", zlog.Context{"exchange": string(id), "thread_start": parent.Of == nil})
	return id, nil
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

// CompleteExchange turns an exchange into a node whose first revision is the
// answer, consuming the exchange in the same transaction.
func (db *DB) CompleteExchange(ctx context.Context, id ExchangeID, markdown string) (Node, error) {
	log := db.log.With(zlog.Context{"exchange": string(id)})
	var node Node
	err := db.inWrite(ctx, log, "zettel.exchange.complete", func(tx *sql.Tx) error {
		pending, err := scanPending(tx.QueryRowContext(ctx, pendingSelect+` WHERE id = ?`, string(id)))
		if errors.Is(err, sql.ErrNoRows) {
			return ErrUnknownExchange
		}
		if err != nil {
			return storagef(err, "reading the exchange")
		}
		now := db.now()
		node = Node{
			ID: NodeID(db.newID()), Thread: pending.Thread, Parent: pending.Parent, Question: pending.Question,
			Answer: Revision{Number: 1, Markdown: markdown, WrittenAt: now}, CreatedAt: now,
		}
		pn, pr, qt, qc := parentColumns(node.Parent)
		if _, err := tx.ExecContext(ctx, `DELETE FROM pending_exchange WHERE id = ?`, string(id)); err != nil {
			return storagef(err, "consuming the exchange")
		}
		if _, err := tx.ExecContext(ctx, `
			INSERT INTO node (id, thread_id, parent_node_id, parent_revision, quote_text, quote_context, question, created_at_ms)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
			string(node.ID), string(node.Thread), pn, pr, qt, qc, node.Question, now.UnixMilli()); err != nil {
			return storagef(err, "inserting the node")
		}
		if _, err := tx.ExecContext(ctx, `INSERT INTO revision (node_id, number, markdown, written_at_ms) VALUES (?, 1, ?, ?)`,
			string(node.ID), markdown, now.UnixMilli()); err != nil {
			return storagef(err, "inserting the first revision")
		}
		return nil
	})
	if err != nil {
		return Node{}, err
	}
	log.Info("zettel.exchange.complete", "an exchange became a node", zlog.Context{"node": string(node.ID), "thread": string(node.Thread)})
	return node, nil
}

// AbandonExchange removes an exchange whose turn ended with no answer.
func (db *DB) AbandonExchange(ctx context.Context, id ExchangeID) error {
	log := db.log.With(zlog.Context{"exchange": string(id)})
	err := db.inWrite(ctx, log, "zettel.exchange.abandon", func(tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM pending_exchange WHERE id = ?`, string(id))
		if err != nil {
			return storagef(err, "removing the exchange")
		}
		n, err := res.RowsAffected()
		if err != nil {
			return storagef(err, "counting the removed exchange")
		}
		if n == 0 {
			return ErrUnknownExchange
		}
		return nil
	})
	if err != nil {
		return err
	}
	log.Info("zettel.exchange.abandon", "an exchange was abandoned", nil)
	return nil
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

// Thread reads one thread whole: its nodes and its exchanges in flight, each
// oldest first.
func (db *DB) Thread(ctx context.Context, thread ThreadID) ([]Node, []Pending, error) {
	log := db.log.With(zlog.Context{"thread": string(thread)})
	rows, err := db.read.QueryContext(ctx, `
		SELECT node.id, node.thread_id, node.parent_node_id, node.parent_revision, node.quote_text, node.quote_context,
		       node.question, node.created_at_ms, node.sightings, revision.number, revision.markdown, revision.written_at_ms
		  FROM node
		  JOIN revision ON revision.node_id = node.id
		   AND revision.number = (SELECT MAX(number) FROM revision r WHERE r.node_id = node.id)
		 WHERE node.thread_id = ?
		 ORDER BY node.created_at_ms, node.id`, string(thread))
	if err != nil {
		log.Error("zettel.thread.read", "reading the thread's nodes failed", zlog.Context{"cause": err.Error()})
		return nil, nil, storagef(err, "reading the thread's nodes")
	}
	nodes, err := collect(rows, scanNode)
	if err != nil {
		log.Error("zettel.thread.read", "reading the thread's nodes failed", zlog.Context{"cause": err.Error()})
		return nil, nil, storagef(err, "reading the thread's nodes")
	}
	pending, err := db.pendingWhere(ctx, ` WHERE thread_id = ?`, string(thread))
	if err != nil {
		log.Error("zettel.thread.read", "reading the thread's exchanges failed", zlog.Context{"cause": err.Error()})
		return nil, nil, err
	}
	return nodes, pending, nil
}

// AllPending reads every exchange in flight, oldest first.
func (db *DB) AllPending(ctx context.Context) ([]Pending, error) {
	pending, err := db.pendingWhere(ctx, "")
	if err != nil {
		db.log.Error("zettel.exchange.list", "reading the exchanges in flight failed", zlog.Context{"cause": err.Error()})
	}
	return pending, err
}

const pendingSelect = `SELECT id, thread_id, turn_id, parent_node_id, parent_revision, quote_text, quote_context, question, begun_at_ms FROM pending_exchange`

func (db *DB) pendingWhere(ctx context.Context, where string, args ...any) ([]Pending, error) {
	rows, err := db.read.QueryContext(ctx, pendingSelect+where+` ORDER BY begun_at_ms, id`, args...)
	if err != nil {
		return nil, storagef(err, "reading exchanges in flight")
	}
	out, err := collect(rows, func(r *sql.Rows) (Pending, error) { return scanPending(r) })
	if err != nil {
		return nil, storagef(err, "reading exchanges in flight")
	}
	return out, nil
}

type scanner interface{ Scan(...any) error }

func scanPending(s scanner) (Pending, error) {
	var p Pending
	var parentNode, quoteText, quoteContext sql.NullString
	var parentRevision sql.NullInt64
	var begun int64
	if err := s.Scan(&p.ID, &p.Thread, &p.Turn, &parentNode, &parentRevision, &quoteText, &quoteContext, &p.Question, &begun); err != nil {
		return Pending{}, err
	}
	p.Parent = parentFromColumns(parentNode, parentRevision, quoteText, quoteContext)
	p.BegunAt = time.UnixMilli(begun)
	return p, nil
}

func scanNode(r *sql.Rows) (Node, error) {
	var n Node
	var parentNode, quoteText, quoteContext sql.NullString
	var parentRevision sql.NullInt64
	var created, written int64
	if err := r.Scan(&n.ID, &n.Thread, &parentNode, &parentRevision, &quoteText, &quoteContext,
		&n.Question, &created, &n.Sightings, &n.Answer.Number, &n.Answer.Markdown, &written); err != nil {
		return Node{}, err
	}
	n.Parent = parentFromColumns(parentNode, parentRevision, quoteText, quoteContext)
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
