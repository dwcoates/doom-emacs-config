package goroutinedump

import (
	"regexp"
	"strconv"
	"strings"
	"sync"
	"testing"
)

// TestRenderNamesTheBlockedGoroutine pins what the dump is FOR: a goroutine
// parked on a channel has to appear in it by name, because that stack is the
// only thing that identifies a wedge's blocking call on a host where no
// debugger can attach.
func TestRenderNamesTheBlockedGoroutine(t *testing.T) {
	// Arrange.
	release := make(chan struct{})
	parked := make(chan struct{})
	go func() {
		close(parked)
		<-release
	}()
	<-parked
	defer close(release)

	// Act.
	dump, count := Render()

	// Assert.
	if count < 2 {
		t.Fatalf("goroutine count = %d, want at least the test's own and the parked one", count)
	}
	if !strings.Contains(dump, "TestRenderNamesTheBlockedGoroutine") {
		t.Fatalf("dump = %q, want the parked goroutine's own stack in it", dump)
	}
}

// TestTruncate pins the cap: a rendering at the cap is kept whole, and one
// past it is cut there and says so rather than silently.
func TestTruncate(t *testing.T) {
	tests := []struct {
		name     string
		in       string
		wantHead int
		wantNote bool
	}{
		{name: "a rendering at the cap is kept whole", in: strings.Repeat("g", Cap), wantHead: Cap},
		{name: "a rendering past the cap is cut and says so", in: strings.Repeat("g", Cap+1), wantHead: Cap, wantNote: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table row.

			// Act.
			got := truncate(tt.in)

			// Assert.
			if head := len(strings.SplitN(got, "\n", 2)[0]); head != tt.wantHead {
				t.Fatalf("kept %d bytes of the rendering, want %d", head, tt.wantHead)
			}
			if noted := strings.Contains(got, "was truncated at"); noted != tt.wantNote {
				t.Fatalf("truncation note present = %v, want %v", noted, tt.wantNote)
			}
		})
	}
}

func TestGroup(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want string
	}{
		{
			name: "goroutines differing only in ids, arguments, offsets and waits are one group",
			in: "goroutine 7 [select, 3 minutes]:\n" +
				"database/sql.(*DB).conn(0x1401226f790, {0x105c22288, 0x1401185e660}, 0x1)\n" +
				"\t/go/src/database/sql/sql.go:1369 +0x49c\n" +
				"created by agent-repld/internal/server.start in goroutine 291\n" +
				"\t/src/server.go:10 +0x28\n\n" +
				"goroutine 9 [select]:\n" +
				"database/sql.(*DB).conn(0x1401226f790, {0x105c22288?, 0x14015c97f50?}, 0x1)\n" +
				"\t/go/src/database/sql/sql.go:1369 +0x4a0\n" +
				"created by agent-repld/internal/server.start in goroutine 4599\n" +
				"\t/src/server.go:10 +0x30",
			want: "2 goroutines [select]: 7, 9\n" +
				"database/sql.(*DB).conn(...)\n" +
				"\t/go/src/database/sql/sql.go:1369\n" +
				"created by agent-repld/internal/server.start\n" +
				"\t/src/server.go:10",
		},
		{
			name: "the same stack in another state is its own group",
			in: "goroutine 1 [running]:\nmain.f(0x1)\n\t/src/main.go:3 +0x1\n\n" +
				"goroutine 2 [chan receive]:\nmain.f(0x2)\n\t/src/main.go:3 +0x1",
			want: "1 goroutine [running]: 1\nmain.f(...)\n\t/src/main.go:3\n\n" +
				"1 goroutine [chan receive]: 2\nmain.f(...)\n\t/src/main.go:3",
		},
		{
			name: "a receiver's (*T) is kept and only the argument list goes",
			in:   "goroutine 3 [running]:\npkg.(*T).Method[...](_, {_, _})\n\t/src/t.go:1 +0x2",
			want: "1 goroutine [running]: 3\npkg.(*T).Method[...](...)\n\t/src/t.go:1",
		},
		{
			name: "a group names its first eight ids and counts them all",
			in: func() string {
				var b []string
				for i := 1; i <= 10; i++ {
					b = append(b, "goroutine "+strconv.Itoa(i)+" [IO wait]:\nnet.read(0x1)\n\t/src/net.go:5 +0x1")
				}
				return strings.Join(b, "\n\n")
			}(),
			want: "10 goroutines [IO wait]: 1, 2, 3, 4, 5, 6, 7, 8, ...\nnet.read(...)\n\t/src/net.go:5",
		},
		{
			name: "a block that is not a goroutine is kept whole, each copy apart",
			in:   "not a goroutine\n\nnot a goroutine",
			want: "not a goroutine\n\nnot a goroutine",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table row.

			// Act.
			got := group(tt.in)

			// Assert.
			if got != tt.want {
				t.Fatalf("group() =\n%s\nwant\n%s", got, tt.want)
			}
		})
	}
}

// TestRenderGroupsIdenticalGoroutines pins what grouping is FOR: many parked
// goroutines on one stack are one entry with their count, so a dump of a
// daemon with thousands of goroutines stays under Cap with every distinct
// stack in it.
func TestRenderGroupsIdenticalGoroutines(t *testing.T) {
	// Arrange.
	const parkedCount = 50
	release := make(chan struct{})
	var ready sync.WaitGroup
	ready.Add(parkedCount)
	for range parkedCount {
		go parkForGrouping(&ready, release)
	}
	ready.Wait()
	defer close(release)

	// Act.
	dump, _ := Render()

	// Assert: a goroutine between its Done and its receive may still be
	// runnable, so the group is one per state it can be caught in, and the
	// counts cover every parked goroutine.
	groups := regexp.MustCompile(`(?m)^(\d+) goroutines? \[[^\]]+\]: [^\n]*\n[^\n]*goroutinedump\.parkForGrouping\(\.\.\.\)$`).FindAllStringSubmatch(dump, -1)
	if len(groups) == 0 || len(groups) > 3 {
		t.Fatalf("parkForGrouping groups = %d, want one per state (at most 3); dump =\n%s", len(groups), dump)
	}
	total := 0
	for _, g := range groups {
		n, err := strconv.Atoi(g[1])
		if err != nil {
			t.Fatalf("group count %q: %v", g[1], err)
		}
		total += n
	}
	if total != parkedCount {
		t.Fatalf("grouped parked goroutines = %d, want %d; dump =\n%s", total, parkedCount, dump)
	}
}

func parkForGrouping(ready *sync.WaitGroup, release <-chan struct{}) {
	ready.Done()
	<-release
}
