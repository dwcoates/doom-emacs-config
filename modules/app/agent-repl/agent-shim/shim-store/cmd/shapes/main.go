// shapes prints the store's residue shape catalog: one row per distinct
// recursive key structure observed on a line no producer stored.
//
// IT IS A DISCOVERY TOOL AND NOTHING READS IT AT RUNTIME. The catalog exists so
// a human can ask what the vendor is emitting that this system does not model,
// and a discovery surface reachable only by `sqlite3` is one nobody consults.
// Run it with `make -C agent-shim/shim-store shapes`.
package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"connectrpc.com/connect"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-store/internal/server"
)

// baseURL is a syntactic requirement of the Connect client; the unix socket is
// what the transport actually dials.
const baseURL = "http://store"

// callTimeout bounds the one rpc this tool makes. The catalog listing is a
// single indexed read of at most a thousand rows, so a store that has not
// answered in five seconds is not answering.
const callTimeout = 5 * time.Second

func main() {
	socket := flag.String("socket", socketDefault(), "the store's unix socket")
	kind := flag.String("kind", "", "show only this residue kind (default: every kind)")
	limit := flag.Uint("limit", 0, "at most this many rows (default: the store's own)")
	example := flag.Bool("example", false, "include the first raw line seen for each shape")
	flag.Parse()

	if err := run(*socket, *kind, uint32(*limit), *example); err != nil {
		fmt.Fprintln(os.Stderr, "shapes:", err)
		os.Exit(1)
	}
}

func run(socket, kind string, limit uint32, example bool) error {
	client := storev1connect.NewShimStoreClient(&http.Client{Transport: &http.Transport{
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, "unix", socket)
		},
	}}, baseURL)

	ctx, cancel := context.WithTimeout(context.Background(), callTimeout)
	defer cancel()

	req := &storev1.ListResidueShapesRequest{Limit: limit, IncludeExample: example}
	// ABSENCE IS THE FIELD OMITTED, never an empty string: the store refuses a
	// present-but-empty kind rather than reading it as "every kind".
	if kind != "" {
		req.Kind = &kind
	}
	res, err := client.ListResidueShapes(ctx, connect.NewRequest(req))
	if err != nil {
		return fmt.Errorf("calling the store on %s: %w", socket, err)
	}
	if failure := res.Msg.GetFailure(); failure != nil {
		return fmt.Errorf("the store refused the listing: %s", failure.GetDetail())
	}
	if err := render(os.Stdout, res.Msg.GetSuccess().GetShapes(), example); err != nil {
		return fmt.Errorf("writing the catalog: %w", err)
	}
	return nil
}

// render writes the catalog to w. An empty catalog says so rather than printing
// nothing, because "no shapes" and "the tool did not run" must not look alike.
//
// THE REPORT IS COMPOSED IN MEMORY AND WRITTEN ONCE, so the one write that can
// fail is the one that is checked: a closed or full stdout is an error the
// tool exits nonzero on, never a truncated listing that looks complete.
func render(w io.Writer, shapes []*storev1.ResidueShapeRow, example bool) error {
	var b strings.Builder
	if len(shapes) == 0 {
		b.WriteString("the residue shape catalog is empty\n")
	}
	for _, s := range shapes {
		hash := s.GetShapeHash()
		if len(hash) > shortHash {
			hash = hash[:shortHash]
		}
		fmt.Fprintf(&b, "%s  count=%d  kind=%s\n", hash, s.GetCount(), s.GetKind())
		fmt.Fprintf(&b, "  first_seen=%s  last_seen=%s\n", millis(s.GetFirstSeenMs()), millis(s.GetLastSeenMs()))
		fmt.Fprintf(&b, "  structure: %s\n", s.GetKeyStructure())
		if example {
			fmt.Fprintf(&b, "  example:   %s\n", s.GetFirstExample())
		}
	}
	if len(shapes) > 0 {
		fmt.Fprintf(&b, "\n%d shape(s)\n", len(shapes))
	}
	_, err := io.WriteString(w, b.String())
	return err
}

// shortHash is how much of a digest identifies a row on screen. Twelve hex
// characters is what every other tool in this repo abbreviates a digest to, and
// the full hash is still what the rpc filters and the table keys on.
const shortHash = 12

func millis(ms int64) string { return time.UnixMilli(ms).Format(time.RFC3339) }

// socketDefault mirrors the store's own --socket default so the tool reaches
// the running store with no argument.
func socketDefault() string {
	if v := os.Getenv(server.EnvSocket); v != "" {
		return v
	}
	base := os.Getenv("XDG_CACHE_HOME")
	if base == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return filepath.Join(".cache", "agent-repl", "sock", "store.sock")
		}
		base = filepath.Join(home, ".cache")
	}
	return filepath.Join(base, "agent-repl", "sock", "store.sock")
}
