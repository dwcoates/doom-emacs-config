package lockwatch

import (
	"sync"
	"testing"
)

func TestMutexHold(t *testing.T) {
	tests := []struct {
		name     string
		act      func(m *Mutex)
		wantHeld bool
	}{
		{name: "a fresh mutex is not held", act: func(*Mutex) {}, wantHeld: false},
		{name: "a locked mutex is held", act: func(m *Mutex) { m.Lock() }, wantHeld: true},
		{name: "an unlocked mutex is not held", act: func(m *Mutex) { m.Lock(); m.Unlock() }, wantHeld: false},
		{name: "a successful TryLock holds it", act: func(m *Mutex) { m.TryLock() }, wantHeld: true},
		{name: "a refused TryLock changes nothing", act: func(m *Mutex) { m.Lock(); m.TryLock(); m.Unlock() }, wantHeld: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			var m Mutex

			// Act.
			tt.act(&m)

			// Assert.
			if _, held := m.hold(); held != tt.wantHeld {
				t.Fatalf("held = %v, want %v", held, tt.wantHeld)
			}
		})
	}
}

// TestMutexDistinguishesTwoHolds pins what the watchdog's episode rests on: a
// release and a re-take between two looks is a DIFFERENT hold, never the
// same one still standing.
func TestMutexDistinguishesTwoHolds(t *testing.T) {
	// Arrange.
	var m Mutex
	m.Lock()
	first, _ := m.hold()

	// Act.
	m.Unlock()
	m.Lock()
	second, held := m.hold()

	// Assert.
	if !held || first == second {
		t.Fatalf("holds = %d then %d (held %v), want two distinct held values", first, second, held)
	}
}

// TestMutexTryLockRefusesWhileHeld pins that TryLock keeps sync.Mutex's answer.
func TestMutexTryLockRefusesWhileHeld(t *testing.T) {
	// Arrange.
	var m Mutex
	m.Lock()

	// Act.
	got := m.TryLock()

	// Assert.
	if got {
		t.Fatal("TryLock took a mutex that was already held")
	}
}

// TestMutexExcludes pins mutual exclusion under contention: the count the
// watchdog reads must end even, and a counter guarded by the mutex must lose
// no increment. Run under -race.
func TestMutexExcludes(t *testing.T) {
	// Arrange.
	const workers, rounds = 8, 1000
	var (
		m       Mutex
		counter int
		wg      sync.WaitGroup
	)

	// Act.
	for range workers {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for range rounds {
				m.Lock()
				counter++
				m.Unlock()
			}
		}()
	}
	wg.Wait()

	// Assert.
	if counter != workers*rounds {
		t.Fatalf("counter = %d, want %d", counter, workers*rounds)
	}
	if h, held := m.hold(); held || h != 2*workers*rounds {
		t.Fatalf("holds = %d (held %v), want %d and not held", h, held, 2*workers*rounds)
	}
}

// BenchmarkLockUnlock measures the hot path the watchdog costs every holder:
// an uncontended Lock/Unlock pair, instrumented against a plain sync.Mutex.
func BenchmarkLockUnlock(b *testing.B) {
	b.Run("sync.Mutex", func(b *testing.B) {
		var m sync.Mutex
		b.ReportAllocs()
		for range b.N {
			m.Lock()
			m.Unlock()
		}
	})
	b.Run("lockwatch.Mutex", func(b *testing.B) {
		var m Mutex
		b.ReportAllocs()
		for range b.N {
			m.Lock()
			m.Unlock()
		}
	})
}

// BenchmarkLockUnlockContended measures the same pair with every P contending.
func BenchmarkLockUnlockContended(b *testing.B) {
	b.Run("sync.Mutex", func(b *testing.B) {
		var m sync.Mutex
		b.ReportAllocs()
		b.RunParallel(func(pb *testing.PB) {
			for pb.Next() {
				m.Lock()
				m.Unlock()
			}
		})
	})
	b.Run("lockwatch.Mutex", func(b *testing.B) {
		var m Mutex
		b.ReportAllocs()
		b.RunParallel(func(pb *testing.PB) {
			for pb.Next() {
				m.Lock()
				m.Unlock()
			}
		})
	})
}
