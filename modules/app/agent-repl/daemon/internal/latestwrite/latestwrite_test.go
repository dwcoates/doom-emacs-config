package latestwrite

import (
	"reflect"
	"sync"
	"testing"
)

// written is one write the Writer made.
type written struct {
	key   string
	value int
}

// recorder is a write that records each write and, while its gate is set,
// waits on the gate before returning; started is told each write begun.
type recorder struct {
	mu      sync.Mutex
	writes  []written
	gate    chan struct{}
	started chan string
}

func (r *recorder) write(key string, value int) {
	if r.started != nil {
		r.started <- key
	}
	if r.gate != nil {
		<-r.gate
	}
	r.mu.Lock()
	r.writes = append(r.writes, written{key, value})
	r.mu.Unlock()
}

func (r *recorder) got() []written {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]written(nil), r.writes...)
}

// later is the merge that keeps the larger value, as a forward-only instant.
func later(held, next int) int { return max(held, next) }

func TestSubmitWritesTheValueOffTheCaller(t *testing.T) {
	// Arrange
	r := &recorder{started: make(chan string, 1)}
	w := New(r.write, later)

	// Act
	w.Submit("ws-1", 5)
	<-r.started
	w.Close()

	// Assert
	if got, want := r.got(), []written{{"ws-1", 5}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("writes = %v, want %v", got, want)
	}
}

func TestSubmitReturnsWhileAWriteIsStillRunning(t *testing.T) {
	// Arrange: the first write is held mid-way.
	r := &recorder{gate: make(chan struct{}), started: make(chan string, 2)}
	w := New(r.write, later)
	w.Submit("ws-1", 1)
	<-r.started

	// Act: a second change is submitted while the first write waits.
	w.Submit("ws-2", 2)

	// Assert: the submit returned with the write still held; releasing it
	// writes both.
	if got := r.got(); len(got) != 0 {
		t.Fatalf("writes = %v, want none finished while the first is held", got)
	}
	close(r.gate)
	w.Close()
	if got, want := r.got(), []written{{"ws-1", 1}, {"ws-2", 2}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("writes = %v, want %v", got, want)
	}
}

func TestAKeySubmittedAgainBeforeItsWriteIsWrittenOnceMerged(t *testing.T) {
	// Arrange: a write is held, so what follows waits as one batch.
	r := &recorder{gate: make(chan struct{}), started: make(chan string, 4)}
	w := New(r.write, later)
	w.Submit("ws-0", 0)
	<-r.started

	// Act
	w.Submit("ws-1", 7)
	w.Submit("ws-1", 3)
	w.Submit("ws-1", 9)
	close(r.gate)
	w.Close()

	// Assert
	if got, want := r.got(), []written{{"ws-0", 0}, {"ws-1", 9}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("writes = %v, want ws-1 written once with the merged value", got)
	}
}

func TestHeldKeysAreWrittenInTheOrderFirstSubmitted(t *testing.T) {
	// Arrange
	r := &recorder{gate: make(chan struct{}), started: make(chan string, 4)}
	w := New(r.write, later)
	w.Submit("ws-0", 0)
	<-r.started

	// Act
	w.Submit("ws-b", 1)
	w.Submit("ws-a", 2)
	w.Submit("ws-b", 3)
	close(r.gate)
	w.Close()

	// Assert
	if got, want := r.got(), []written{{"ws-0", 0}, {"ws-b", 3}, {"ws-a", 2}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("writes = %v, want %v", got, want)
	}
}

func TestCloseWritesEverythingHeldBeforeItReturns(t *testing.T) {
	// Arrange: a write is held, and two more changes wait behind it.
	r := &recorder{gate: make(chan struct{}), started: make(chan string, 3)}
	w := New(r.write, later)
	w.Submit("ws-1", 1)
	<-r.started
	w.Submit("ws-2", 2)
	w.Submit("ws-3", 3)
	closed := make(chan struct{})

	// Act
	go func() {
		w.Close()
		close(closed)
	}()
	close(r.gate)
	<-closed

	// Assert
	if got, want := r.got(), []written{{"ws-1", 1}, {"ws-2", 2}, {"ws-3", 3}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("writes = %v, want every held change written by Close", got)
	}
}

func TestASubmitAfterCloseWritesOnItsCaller(t *testing.T) {
	// Arrange
	r := &recorder{}
	w := New(r.write, later)
	w.Close()

	// Act
	w.Submit("ws-1", 4)

	// Assert: written before Submit returned.
	if got, want := r.got(), []written{{"ws-1", 4}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("writes = %v, want %v", got, want)
	}
}

func TestASecondCloseReturns(t *testing.T) {
	// Arrange
	r := &recorder{}
	w := New(r.write, later)
	w.Submit("ws-1", 1)
	w.Close()

	// Act
	w.Close()

	// Assert
	if got, want := r.got(), []written{{"ws-1", 1}}; !reflect.DeepEqual(got, want) {
		t.Fatalf("writes = %v, want %v", got, want)
	}
}
