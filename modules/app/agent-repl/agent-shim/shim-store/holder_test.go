package main

import (
	"bytes"
	"context"
	"go/ast"
	"go/parser"
	"go/token"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"agentrepl/logging/sockethold"
	"agentrepl/shim-store/internal/logging"
)

// testHolder serves NAMES from a holder in this test, answering its control
// socket's path and the data sockets' paths by name.
func testHolder(t *testing.T, names ...string) (string, map[string]string) {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "sth")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	paths := map[string]string{}
	files := map[string]*os.File{}
	for _, name := range names {
		path := filepath.Join(dir, name+".sock")
		ln, err := net.ListenUnix("unix", &net.UnixAddr{Name: path, Net: "unix"})
		if err != nil {
			t.Fatal(err)
		}
		ln.SetUnlinkOnClose(false)
		f, err := ln.File()
		if err != nil {
			t.Fatal(err)
		}
		ln.Close()
		paths[name], files[name] = path, f
	}
	control := filepath.Join(dir, "c.sock")
	controlLn, err := net.ListenUnix("unix", &net.UnixAddr{Name: control, Net: "unix"})
	if err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	var wg sync.WaitGroup
	wg.Add(1)
	go func() { defer wg.Done(); _ = sockethold.Serve(ctx, controlLn, files, func(sockethold.Grant) {}) }()
	t.Cleanup(func() {
		cancel()
		wg.Wait()
		for _, f := range files {
			f.Close()
		}
	})
	return control, paths
}

func acceptsOn(t *testing.T, ln net.Listener, path string) {
	t.Helper()
	c, err := net.Dial("unix", path)
	if err != nil {
		t.Fatalf("dial %s: %v", path, err)
	}
	defer c.Close()
	conn, err := ln.Accept()
	if err != nil {
		t.Fatalf("accept: %v", err)
	}
	conn.Close()
}

func TestStoreListenersAreTakenFromTheHolder(t *testing.T) {
	// Arrange
	control, paths := testHolder(t, holderStoreSocket, holderReadSocket)
	sink := &bytes.Buffer{}
	log := logging.New(sink, &bytes.Buffer{}, true)

	// Act
	ln, readLn, err := storeListeners(control, "/unused/store.sock", "/unused/store-read.sock", log, log)

	// Assert
	if err != nil {
		t.Fatalf("storeListeners: %v", err)
	}
	defer ln.Close()
	defer readLn.Close()
	acceptsOn(t, ln, paths[holderStoreSocket])
	acceptsOn(t, readLn, paths[holderReadSocket])
	if !hasOperation(storeRecords(t, sink), "store.listen.holder") {
		t.Fatalf("records = %+v, want each take recorded", storeRecords(t, sink))
	}
}

func TestStoreListenersFailTheBootWhenTheHolderCannotGrant(t *testing.T) {
	cases := []struct {
		name    string
		control func(t *testing.T) string
	}{
		{name: "a holder holding no read-only socket", control: func(t *testing.T) string {
			c, _ := testHolder(t, holderStoreSocket)
			return c
		}},
		{name: "no holder at the control path", control: func(t *testing.T) string { return "/tmp/shim-store-no-holder.sock" }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			sink := &bytes.Buffer{}
			log := logging.New(sink, &bytes.Buffer{}, true)

			// Act
			_, _, err := storeListeners(tc.control(t), "/unused/a.sock", "/unused/b.sock", log, log)

			// Assert
			if err == nil {
				t.Fatal("storeListeners = nil, want the boot failed")
			}
			found := false
			for _, rec := range storeRecords(t, sink) {
				if rec.Operation == "store.listen.holder" && rec.Level == "error" {
					found = true
				}
			}
			if !found {
				t.Fatalf("records = %+v, want the failed take at ERROR", storeRecords(t, sink))
			}
		})
	}
}

func TestLockDatabaseAdmitsOneStorePerDatabase(t *testing.T) {
	// Arrange
	db := filepath.Join(t.TempDir(), "events.db")
	sink := &bytes.Buffer{}
	log := logging.New(sink, &bytes.Buffer{}, true)
	release, err := lockDatabase(db, log)
	if err != nil {
		t.Fatalf("first lock: %v", err)
	}

	// Act
	_, second := lockDatabase(db, log)
	release()
	again, third := lockDatabase(db, log)

	// Assert
	if second == nil {
		t.Fatal("a second store's lock = nil, want it refused")
	}
	if third != nil {
		t.Fatalf("a lock after the release = %v, want it taken", third)
	}
	again()
	found := false
	for _, rec := range storeRecords(t, sink) {
		if rec.Operation == "store.lock" && rec.Level == "error" {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want the refused second store at ERROR", storeRecords(t, sink))
	}
}

// TestTheStoresHolderTakeIsSocketholdsTimedTake pins the one bounded, timed holder take: the store calls
// sockethold.TakeTimed and keeps no take bound of its own.
func TestTheStoresHolderTakeIsSocketholdsTimedTake(t *testing.T) {
	// Arrange
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatal(err)
	}
	fset := token.NewFileSet()
	calls, ownBound := 0, ""

	// Act
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".go") || strings.HasSuffix(e.Name(), "_test.go") {
			continue
		}
		f, err := parser.ParseFile(fset, e.Name(), nil, 0)
		if err != nil {
			t.Fatal(err)
		}
		ast.Inspect(f, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.ValueSpec:
				for _, id := range n.Names {
					if strings.Contains(id.Name, "TakeBound") || strings.Contains(id.Name, "takeBound") {
						ownBound = fset.Position(id.Pos()).String()
					}
				}
			case *ast.SelectorExpr:
				if id, ok := n.X.(*ast.Ident); ok && id.Name == "sockethold" && n.Sel.Name == "TakeTimed" {
					calls++
				}
			}
			return true
		})
	}

	// Assert
	if ownBound != "" || calls != 1 {
		t.Fatalf("own take bound at %q, %d sockethold.TakeTimed calls; want none of its own and exactly one call", ownBound, calls)
	}
}
