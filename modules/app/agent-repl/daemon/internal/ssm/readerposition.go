package ssm

import (
	"database/sql"
	"errors"
	"fmt"
)

// THE READER'S PLACE IN A CONVERSATION, and the reason it lives HERE rather
// than in the client that reads.
//
// conversation-history.proto's whole contract is that THE CLIENT CANNOT NAME A
// POSITION: not a seq, not an offset, not a cursor it authored or holds.
// NextPageCmd carries no position at all. Something still has to know where the
// last page ended, and this is it — the daemon's own record, keyed PER READER
// PER WORKSPACE, so two tabs scrolled to different depths of the same
// conversation cannot read each other's place, and one tab scrolled in two
// workspaces keeps two.
//
// # The generation is stored WITH the position, and that is what replaces a fence
//
// A fence was daemon state the client echoed back so the daemon could check the
// client against itself. There is nothing to echo here: the position is the
// daemon's, so the generation it was established under is the daemon's too, and
// a rotation is detected by comparing the daemon's stored token against the
// daemon's live one. On mismatch the row is DROPPED, the next NextPageCmd
// arrives with no position, and it is REFUSED — which is the recovery story the
// contract names, and the client answers it with FirstPageCmd.
//
// Storing the generation is what makes the drop STRUCTURAL rather than an edge
// somebody must remember to hook: there is no rotation path that can forget to
// invalidate, because invalidation is a comparison made on every read.
//
// # Every row is dropped at Open, on purpose
//
// A reader is a live frontend connection, and no connection survives a daemon
// bounce. A row that outlived the daemon names a reader that cannot come back,
// and worse, connection ids restart — so a surviving row could be inherited by
// an unrelated later connection and turn a cold open into a silent read of
// somebody else's place. Clearing at Open makes that unrepresentable.

// ConversationReaderPosition is one reader's place in one workspace's history.
type ConversationReaderPosition struct {
	// GenerationID is the controller generation the position was established
	// under. A position whose generation is not the live one is stale and is
	// dropped rather than served.
	GenerationID string
	// BeforeSeq is the EXCLUSIVE upper bound of the NEXT page this reader is
	// owed: the oldest seq the last page served to it covered.
	BeforeSeq uint64
}

// ErrNoConversationReaderPosition reports a reader that has no established
// position for the workspace. It is a REFUSAL, never a cue to serve the tail:
// "I have no position" already has its own verb (FirstPageCmd).
var ErrNoConversationReaderPosition = errors.New("ssm: this reader has no established position in the workspace's conversation")

// ConversationReaderPosition reads the reader's place, reporting absence as a
// distinct fact rather than as a zero position.
func (m *Manager) ConversationReaderPosition(reader, workspace string) (ConversationReaderPosition, bool, error) {
	if reader == "" {
		return ConversationReaderPosition{}, false, fmt.Errorf("ssm: ConversationReaderPosition got an empty reader for workspace %q", workspace)
	}
	if workspace == "" {
		return ConversationReaderPosition{}, false, fmt.Errorf("ssm: ConversationReaderPosition for reader %q got an empty workspace", reader)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	var (
		generation string
		beforeSeq  int64
	)
	err := m.db.QueryRow(`
		SELECT generation_id, before_seq FROM conversation_reader_position
		WHERE reader = ? AND workspace = ?
	`, reader, workspace).Scan(&generation, &beforeSeq)
	if errors.Is(err, sql.ErrNoRows) {
		return ConversationReaderPosition{}, false, nil
	}
	if err != nil {
		return ConversationReaderPosition{}, false, fmt.Errorf("ssm: read conversation reader position reader=%q ws=%q: %w", reader, workspace, err)
	}
	return ConversationReaderPosition{GenerationID: generation, BeforeSeq: uint64(beforeSeq)}, true, nil
}

// SetConversationReaderPosition records where the page just served ends, so the
// NEXT page continues from it. It REPLACES whatever the reader held, which is
// what makes FirstPageCmd a reset rather than a second position.
func (m *Manager) SetConversationReaderPosition(reader, workspace, generationID string, beforeSeq uint64) error {
	if reader == "" {
		return fmt.Errorf("ssm: SetConversationReaderPosition got an empty reader for workspace %q", workspace)
	}
	if workspace == "" {
		return fmt.Errorf("ssm: SetConversationReaderPosition for reader %q got an empty workspace", reader)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	at := m.nextAt()
	if _, err := m.db.Exec(`
		INSERT INTO conversation_reader_position (reader, workspace, generation_id, before_seq, at)
		VALUES (?, ?, ?, ?, ?)
		ON CONFLICT(reader, workspace) DO UPDATE SET
			generation_id = excluded.generation_id,
			before_seq    = excluded.before_seq,
			at            = excluded.at
	`, reader, workspace, generationID, int64(beforeSeq), at); err != nil {
		return fmt.Errorf("ssm: write conversation reader position reader=%q ws=%q before_seq=%d: %w", reader, workspace, beforeSeq, err)
	}
	return nil
}

// DropConversationReaderPosition forgets the reader's place. The next
// NextPageCmd from it is refused, which is exactly the intent: the position it
// would have continued from no longer names anything.
func (m *Manager) DropConversationReaderPosition(reader, workspace string) error {
	if reader == "" {
		return fmt.Errorf("ssm: DropConversationReaderPosition got an empty reader for workspace %q", workspace)
	}
	if workspace == "" {
		return fmt.Errorf("ssm: DropConversationReaderPosition for reader %q got an empty workspace", reader)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if _, err := m.db.Exec(`
		DELETE FROM conversation_reader_position WHERE reader = ? AND workspace = ?
	`, reader, workspace); err != nil {
		return fmt.Errorf("ssm: drop conversation reader position reader=%q ws=%q: %w", reader, workspace, err)
	}
	return nil
}

// clearConversationReaderPositionsLocked discards every persisted position at
// Open. See the header: no reader survives the daemon, and a surviving row can
// only ever be inherited by the wrong one.
func (m *Manager) clearConversationReaderPositionsLocked() error {
	res, err := m.db.Exec(`DELETE FROM conversation_reader_position`)
	if err != nil {
		return fmt.Errorf("ssm: clear conversation reader positions at open: %w", err)
	}
	if n, err := res.RowsAffected(); err == nil && n > 0 {
		m.logf("ssm: cleared %d conversation reader position(s) at open — no frontend connection survives a daemon bounce, so every stored place names a reader that cannot return", n)
	}
	return nil
}
