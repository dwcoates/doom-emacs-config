package db

import (
	"errors"
	"path/filepath"
	"strconv"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
)

// turnPromptLine is the prompt line of `turn` in `book`, stamped with it and
// placed at `atMs`, as the shim writes a turn's opening prompt.
func turnPromptLine(book, turn string, atMs int64) *storev1.StoreEntry {
	item := &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_AgentPrompt{AgentPrompt: &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: turn},
		Agent: &conversationv1.AgentId{Value: book},
	}}}
	key := "prompt-" + book + "-" + turn
	return placed(stampedTurn(pageEntry("w-"+key, key, book, item), turn), atMs, 0)
}

// turnStepLine is one step of `turn` in `book`, placed at `atMs`.
func turnStepLine(book, turn string, atMs int64) *storev1.StoreEntry {
	seedUnit++
	key := "step-" + book + "-" + strconv.Itoa(seedUnit)
	return placed(stampedTurn(pageEntry("w-"+key, key, book, frameItem(activityFrame(book, "act-"+key, prose()))), turn), atMs, 0)
}

// writeSteps writes `n` steps of `turn` at consecutive places from `fromMs`,
// and returns the pointer of the last one written.
func writeSteps(t *testing.T, d *DB, book, turn string, fromMs int64, n int) string {
	t.Helper()
	var last string
	for i := 0; i < n; i++ {
		last = pointerOf(t, writeOK(t, d, turnStepLine(book, turn, fromMs+int64(i))))
	}
	return last
}

// headPointers renders a page's turn heads as their prompt lines' pointers, in
// served order.
func headPointers(heads []*storev1.StoreTurnHead) []string {
	lines := make([]*storev1.StoreLineAt, 0, len(heads))
	for _, head := range heads {
		lines = append(lines, head.GetLine())
	}
	return servedPointers(lines)
}

// stepLineAt is one step of `turn` in `book` at a stated place and ordinal.
func stepLineAt(book, turn string, atMs int64, ordinal uint32) *storev1.StoreEntry {
	entry := turnStepLine(book, turn, atMs)
	entry.Place.Ordinal = ordinal
	return entry
}

func TestEveryPageCarriesTheTurnHeadsBelowIt(t *testing.T) {
	tests := []struct {
		name string
		// arrange writes the book and returns the pointers of the heads the
		// page must carry, newest first, and the page's read.
		arrange func(t *testing.T, d *DB) (want []string, read func() ([]*storev1.StoreTurnHead, error))
	}{
		{
			name: "a turn whose prompt is on the page carries no head",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeSteps(t, d, "agent-1", "turn-a", 101, 3)
				return nil, repaintHeads(d)
			},
		},
		{
			name: "a turn whose prompt is below the repainted page is headed by it",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				prompt := pointerOf(t, writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100)))
				writeSteps(t, d, "agent-1", "turn-a", 101, PageSize)
				return []string{prompt}, repaintHeads(d)
			},
		},
		{
			name: "several turns below the page are headed newest first",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				older := pointerOf(t, writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100)))
				newer := pointerOf(t, writeOK(t, d, turnPromptLine("agent-1", "turn-b", 200)))
				writeSteps(t, d, "agent-1", "turn-b", 201, PageSize)
				writeSteps(t, d, "agent-1", "turn-a", 1000, 1)
				return []string{newer, older}, repaintHeads(d)
			},
		},
		{
			name: "a turn the store holds no prompt line for carries no head",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				writeSteps(t, d, "agent-1", "turn-a", 100, PageSize+1)
				return nil, repaintHeads(d)
			},
		},
		{
			name: "a prompt of another book is no head",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				writeOK(t, d, turnPromptLine("agent-2", "turn-a", 100))
				writeSteps(t, d, "agent-2", "turn-a", 101, 1)
				writeSteps(t, d, "agent-1", "turn-a", 200, 2)
				return nil, repaintHeads(d)
			},
		},
		{
			name: "a continuation page is headed by a prompt below it",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				prompt := pointerOf(t, writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100)))
				writeSteps(t, d, "agent-1", "turn-a", 101, PageSize)
				after := writeSteps(t, d, "agent-1", "turn-a", 1000, 1)
				return []string{prompt}, afterHeads(d, after)
			},
		},
		{
			name: "the floor page carries no head",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				writeSteps(t, d, "agent-1", "turn-a", 100, 2)
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 500))
				after := writeSteps(t, d, "agent-1", "turn-a", 1000, 1)
				return nil, afterHeads(d, after)
			},
		},
		{
			name: "a through page is headed by a prompt below it",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				prompt := pointerOf(t, writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100)))
				writeSteps(t, d, "agent-1", "turn-a", 101, PageSize)
				writeSteps(t, d, "agent-1", "turn-a", 1000, 1)
				return []string{prompt}, func() ([]*storev1.StoreTurnHead, error) {
					page, err := d.ReadPageThrough(ctx(), "agent-1", 500)
					return page.GetHeads(), err
				}
			},
		},
		{
			name: "a known_through opening is headed by a prompt below it",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				prompt := pointerOf(t, writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100)))
				mark := writeSteps(t, d, "agent-1", "turn-a", 101, 1)
				writeSteps(t, d, "agent-1", "turn-a", 200, 2)
				return []string{prompt}, func() ([]*storev1.StoreTurnHead, error) {
					opened, err := d.OpenPage(ctx(), "agent-1", CatchUp(&storev1.StoreItemPointer{Value: mark}))
					return opened.Page.GetHeads(), err
				}
			},
		},
		{
			name: "a tail-only opening carries no head",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeSteps(t, d, "agent-1", "turn-a", 101, PageSize)
				return nil, func() ([]*storev1.StoreTurnHead, error) {
					opened, err := d.OpenPage(ctx(), "agent-1", TailOnly())
					return opened.Page.GetHeads(), err
				}
			},
		},
		{
			name: "a retired prompt line is no head",
			arrange: func(t *testing.T, d *DB) ([]string, func() ([]*storev1.StoreTurnHead, error)) {
				result := writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeSteps(t, d, "agent-1", "turn-a", 101, PageSize)
				if _, err := d.sql.Exec(`UPDATE entry SET kind = ? WHERE position = ?`, kindRetired, positionOf(t, result)); err != nil {
					t.Fatalf("retiring the prompt: %v", err)
				}
				return nil, repaintHeads(d)
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)
			want, read := test.arrange(t, d)

			// Act
			heads, err := read()

			// Assert
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			if got := headPointers(heads); !slicesEqual(got, want) {
				t.Fatalf("turn heads = %v, want %v", got, want)
			}
		})
	}
}

func repaintHeads(d *DB) func() ([]*storev1.StoreTurnHead, error) {
	return func() ([]*storev1.StoreTurnHead, error) {
		opened, err := d.OpenPage(ctx(), "agent-1", Repaint())
		return opened.Page.GetHeads(), err
	}
}

func afterHeads(d *DB, after string) func() ([]*storev1.StoreTurnHead, error) {
	return func() ([]*storev1.StoreTurnHead, error) {
		page, err := d.ReadPage(ctx(), "agent-1", &storev1.StoreItemPointer{Value: after})
		return page.GetHeads(), err
	}
}

// positionOf is the row position of a write's one published line.
func positionOf(t *testing.T, result WriteResult) int64 {
	t.Helper()
	position, err := decodePointer(&storev1.StoreItemPointer{Value: pointerOf(t, result)}, "test")
	if err != nil {
		t.Fatalf("decodePointer: %v", err)
	}
	return position
}

func TestATurnHeadIsTheServedPromptLineWithItsPointerTurnAndPlace(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	written := writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100)).Lines[0].Line
	writeSteps(t, d, "agent-1", "turn-a", 101, PageSize)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", Repaint())

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if heads := opened.Page.GetHeads(); len(heads) != 1 || !proto.Equal(heads[0].GetLine(), written) {
		t.Fatalf("turn heads = %v, want exactly the prompt line as served %v", heads, written)
	}
}

func TestTheTurnPromptIndexHoldsAStampedPromptLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))

	// Assert
	if got := scalar[string](t, d, `SELECT turn_id FROM turn_prompt WHERE position = ?`, positionOf(t, result)); got != "turn-a" {
		t.Fatalf("turn_prompt turn = %q, want %q", got, "turn-a")
	}
}

func TestTheTurnPromptIndexHoldsNoStepLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	writeSteps(t, d, "agent-1", "turn-a", 100, 1)

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM turn_prompt`); got != 0 {
		t.Fatalf("turn_prompt rows = %d, want none for a step", got)
	}
}

func TestTheTurnPromptIndexHoldsNoUnstampedPromptLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := turnPromptLine("agent-1", "turn-a", 100)
	entry.Turn = nil

	// Act
	writeOK(t, d, entry)

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM turn_prompt`); got != 0 {
		t.Fatalf("turn_prompt rows = %d, want none for a prompt no write stamped", got)
	}
}

func TestTheTurnPromptIndexRecordsATurnAPromptLineGainsLater(t *testing.T) {
	// Arrange: the first write states no turn; a later write of the row does.
	d, _ := newStore(t)
	first := turnPromptLine("agent-1", "turn-a", 100)
	first.Turn = nil
	result := writeOK(t, d, first)
	later := turnPromptLine("agent-1", "turn-a", 100)
	later.WriteId = "w-later"

	// Act
	writeOK(t, d, later)

	// Assert
	if got := scalar[string](t, d, `SELECT turn_id FROM turn_prompt WHERE position = ?`, positionOf(t, result)); got != "turn-a" {
		t.Fatalf("turn_prompt turn = %q, want the gained %q", got, "turn-a")
	}
}

// preTurnPromptDatabase writes a database carrying one prompt line and one step
// but NOT turn_prompt — the shape the owner's events.db had before turn heads —
// and returns its path, closed. `damage` runs on the open handle before close.
func preTurnPromptDatabase(t *testing.T, damage func(d *DB)) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	d, err := OpenWithOptions(path, log, Options{unsynced: true, Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
	writeSteps(t, d, "agent-1", "turn-a", 101, 1)
	if _, err := d.sql.Exec(`DROP TABLE turn_prompt`); err != nil {
		t.Fatalf("dropping turn_prompt: %v", err)
	}
	damage(d)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	return path
}

func TestOpenIndexesEveryStoredPromptLineWhenItBuildsTheTurnPromptIndex(t *testing.T) {
	// Arrange
	path := preTurnPromptDatabase(t, func(*DB) {})
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{unsynced: true})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert
	if got := scalar[string](t, d, `SELECT book_agent_id || '/' || turn_id FROM turn_prompt`); got != "agent-1/turn-a" {
		t.Fatalf("turn_prompt = %q, want the stored prompt line indexed", got)
	}
}

func TestOpenRefusesToBuildTheTurnPromptIndexOverAFrameItCannotDecode(t *testing.T) {
	// Arrange
	path := preTurnPromptDatabase(t, func(d *DB) {
		if _, err := d.sql.Exec(`UPDATE entry SET frame = x'ff' WHERE kind = ?`, kindPageLine); err != nil {
			t.Fatalf("damaging the frames: %v", err)
		}
	})
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{unsynced: true})

	// Assert
	if err == nil {
		d.Close() //nolint:errcheck // test teardown
		t.Fatalf("reopen succeeded; want the backfill's decode failure")
	}
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("reopen error = %v, want a storage failure", err)
	}
}

func TestEachTurnHeadSaysWhetherStepsSitBetweenItAndThePage(t *testing.T) {
	tests := []struct {
		name string
		// arrange writes book agent-1, whose repainted page carries exactly one
		// head, that of turn-a.
		arrange func(t *testing.T, d *DB)
		want    bool // steps_between; adjacent when false
	}{
		{
			name: "a page whose lowest line follows the prompt directly is adjacent",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeSteps(t, d, "agent-1", "turn-a", 101, PageSize)
			},
			want: false,
		},
		{
			name: "a step of the turn below the page is steps between",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeSteps(t, d, "agent-1", "turn-a", 101, PageSize+1)
			},
			want: true,
		},
		{
			name: "only lines of another turn between is adjacent",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeSteps(t, d, "agent-1", "turn-b", 101, 2)
				writeSteps(t, d, "agent-1", "turn-a", 200, PageSize)
			},
			want: false,
		},
		{
			name: "a line of the same turn id in another book between is adjacent",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeSteps(t, d, "agent-2", "turn-a", 101, 1)
				writeSteps(t, d, "agent-1", "turn-a", 200, PageSize)
			},
			want: false,
		},
		{
			name: "a retired step between is adjacent",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				retired := writeOK(t, d, turnStepLine("agent-1", "turn-a", 101))
				writeSteps(t, d, "agent-1", "turn-a", 200, PageSize)
				if _, err := d.sql.Exec(`UPDATE entry SET kind = ? WHERE position = ?`, kindRetired, positionOf(t, retired)); err != nil {
					t.Fatalf("retiring the step: %v", err)
				}
			},
			want: false,
		},
		{
			name: "a step written after the page but placed between is steps between",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeSteps(t, d, "agent-1", "turn-a", 200, PageSize)
				writeSteps(t, d, "agent-1", "turn-a", 150, 1)
			},
			want: true,
		},
		{
			name: "a step at the prompt's instant ranked after it is steps between",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				writeOK(t, d, stepLineAt("agent-1", "turn-a", 100, 1))
				writeSteps(t, d, "agent-1", "turn-a", 200, PageSize)
			},
			want: true,
		},
		{
			name: "a step at the prompt's instant ranked before it is adjacent",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, stepLineAt("agent-1", "turn-a", 50, 0))
				promptAt := turnPromptLine("agent-1", "turn-a", 100)
				promptAt.Place.Ordinal = 1
				writeOK(t, d, promptAt)
				writeOK(t, d, stepLineAt("agent-1", "turn-a", 100, 0))
				writeSteps(t, d, "agent-1", "turn-a", 200, PageSize)
			},
			want: false,
		},
		{
			name: "a step whose first stated place moved it between is steps between",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
				unplaced := turnStepLine("agent-1", "turn-a", 0)
				unplaced.Place = nil
				writeOK(t, d, unplaced)
				writeSteps(t, d, "agent-1", "turn-a", testNow+1000, PageSize)
				gained := proto.Clone(unplaced).(*storev1.StoreEntry)
				gained.WriteId = "w-gained"
				gained.Place = &conversationv1.ConversationPlace{AtMs: 150}
				writeOK(t, d, gained)
			},
			want: true,
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)
			test.arrange(t, d)

			// Act
			heads, err := repaintHeads(d)()

			// Assert
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			if len(heads) != 1 {
				t.Fatalf("heads = %v, want exactly turn-a's", heads)
			}
			if got := heads[0].GetStepsBetween() != nil; got != test.want {
				t.Fatalf("steps_between = %t (head %v), want %t", got, heads[0], test.want)
			}
			if heads[0].GetBetween() == nil {
				t.Fatalf("head %v carries no between arm", heads[0])
			}
		})
	}
}

func TestATurnHeadAnswersItsBetweenOnAContinuationPage(t *testing.T) {
	// Arrange: the continuation below the newest page holds the turn's lowest
	// steps, one of which still sits below it.
	d, _ := newStore(t)
	writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
	writeSteps(t, d, "agent-1", "turn-a", 101, 1)
	after := writeSteps(t, d, "agent-1", "turn-a", 200, PageSize+1)
	writeSteps(t, d, "agent-1", "turn-a", 1000, 1)

	// Act
	page, err := d.ReadPage(ctx(), "agent-1", &storev1.StoreItemPointer{Value: after})

	// Assert
	if err != nil {
		t.Fatalf("ReadPage: %v", err)
	}
	if heads := page.GetHeads(); len(heads) != 1 || heads[0].GetStepsBetween() == nil {
		t.Fatalf("heads = %v, want turn-a's with steps between", heads)
	}
}

func TestTheTurnLineIndexHoldsAStampedStepWithItsPlace(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, stepLineAt("agent-1", "turn-a", 100, 3))

	// Assert
	if got := scalar[string](t, d, `SELECT turn_id || '@' || at_ms || '.' || ordinal FROM turn_line WHERE position = ?`, positionOf(t, result)); got != "turn-a@100.3" {
		t.Fatalf("turn_line = %q, want %q", got, "turn-a@100.3")
	}
}

func TestTheTurnLineIndexHoldsNoUnstampedLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	entry := turnStepLine("agent-1", "turn-a", 100)
	entry.Turn = nil

	// Act
	writeOK(t, d, entry)

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM turn_line`); got != 0 {
		t.Fatalf("turn_line rows = %d, want none for a line no write stamped", got)
	}
}

func TestTheTurnLineIndexRestatesAPlaceTheLineGainsLater(t *testing.T) {
	// Arrange: the first write states no place; a later write of the row does.
	d, _ := newStore(t)
	first := turnStepLine("agent-1", "turn-a", 0)
	first.Place = nil
	result := writeOK(t, d, first)
	later := proto.Clone(first).(*storev1.StoreEntry)
	later.WriteId = "w-later"
	later.Place = &conversationv1.ConversationPlace{AtMs: 150, Ordinal: 2}

	// Act
	writeOK(t, d, later)

	// Assert
	if got := scalar[string](t, d, `SELECT at_ms || '.' || ordinal FROM turn_line WHERE position = ?`, positionOf(t, result)); got != "150.2" {
		t.Fatalf("turn_line place = %q, want the gained %q", got, "150.2")
	}
}

// preTurnLineDatabase writes a database carrying one prompt line and one step
// but NOT turn_line (the shape the owner's events.db had before heads said
// what sits between) and returns its path, closed. `damage` runs on the open
// handle before close.
func preTurnLineDatabase(t *testing.T, damage func(d *DB)) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	d, err := OpenWithOptions(path, log, Options{unsynced: true, Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	writeOK(t, d, turnPromptLine("agent-1", "turn-a", 100))
	writeSteps(t, d, "agent-1", "turn-a", 101, 1)
	if _, err := d.sql.Exec(`DROP TABLE turn_line`); err != nil {
		t.Fatalf("dropping turn_line: %v", err)
	}
	damage(d)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	return path
}

func TestOpenIndexesEveryStoredStampedLineWhenItBuildsTheTurnLineIndex(t *testing.T) {
	// Arrange
	path := preTurnLineDatabase(t, func(*DB) {})
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{unsynced: true})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert
	if got := scalar[string](t, d, `SELECT group_concat(turn_id || '@' || at_ms, ',') FROM (SELECT turn_id, at_ms FROM turn_line ORDER BY at_ms)`); got != "turn-a@100,turn-a@101" {
		t.Fatalf("turn_line = %q, want the stored prompt and step indexed with their places", got)
	}
}

func TestOpenRefusesToBuildTheTurnLineIndexOverAFrameItCannotDecode(t *testing.T) {
	// Arrange
	path := preTurnLineDatabase(t, func(d *DB) {
		if _, err := d.sql.Exec(`UPDATE entry SET frame = x'ff' WHERE kind = ?`, kindPageLine); err != nil {
			t.Fatalf("damaging the frames: %v", err)
		}
	})
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{unsynced: true})

	// Assert
	if err == nil {
		d.Close() //nolint:errcheck // test teardown
		t.Fatalf("reopen succeeded; want the backfill's decode failure")
	}
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("reopen error = %v, want a storage failure", err)
	}
}

func TestOpenRefusesToBuildTheTurnLineIndexOverALineWithNoPlace(t *testing.T) {
	// Arrange
	path := preTurnLineDatabase(t, func(d *DB) {
		if _, err := d.sql.Exec(`DELETE FROM entry_place`); err != nil {
			t.Fatalf("damaging the order index: %v", err)
		}
	})
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{unsynced: true})

	// Assert
	if err == nil {
		d.Close() //nolint:errcheck // test teardown
		t.Fatalf("reopen succeeded; want the missing place refused")
	}
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("reopen error = %v, want a storage failure", err)
	}
}

func TestTheTurnHeadsStatementSeeksTheTurnPromptIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, expandInList(turnHeadsSQL, 2), testNow, 0, 100, kindPageLine, "agent-1", "turn-a", "turn-b", kindPageLine, testNow, 0, 100)

	// Assert
	assertNoTableScan(t, "the turn heads of a page", plan)
}
