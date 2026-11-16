// Command zettel is the knowledge graph service: a singleton, launchd-managed
// process that alone owns zettel's SQLite database and serves zettel.v1.Zettel
// to the agent-repl daemon over a unix domain socket with Connect.
//
// Flags (every path is injectable, so a test can point it at a temp dir):
//
//	--socket  UDS path to serve on  (env AGENT_REPL_ZETTEL_SOCKET, else ~/.cache/agent-repl/sock/zettel.sock)
//	--db      graph database path   (~/zettel/zettel.db — the user's data, beside their threads)
//	--log     size-capped rotating log file (~/.cache/agent-repl/log/zettel.log)
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"

	"agentrepl/logging/buildreport"
	"agentrepl/logging/recordlog"
	"agentrepl/zettel/internal/graph"
	"agentrepl/zettel/internal/server"
	"agentrepl/zettel/internal/zlog"
)

// shutdownGrace bounds the orderly drain of in-flight requests.
const shutdownGrace = 5 * time.Second

func main() {
	home, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "zettel: resolving the home directory failed: %v\n", err)
		os.Exit(1)
	}
	cache := cacheDir(home)
	socketPath := flag.String("socket", envOr(server.EnvSocket, filepath.Join(cache, "sock", "zettel.sock")), "UDS path to serve zettel.v1.Zettel on")
	dbPath := flag.String("db", filepath.Join(home, "zettel", "zettel.db"), "graph database path")
	logPath := flag.String("log", filepath.Join(cache, "log", "zettel.log"), "log file path (size-capped, rotated)")
	flag.Parse()
	if err := run(*socketPath, *dbPath, *logPath); err != nil {
		os.Exit(1)
	}
}

// run opens the log, reports the build, opens the graph and serves until a
// termination signal. Every failure after the log opens is logged before it
// returns; a failure to open the log itself goes to stderr.
func run(socketPath, dbPath, logPath string) (err error) {
	log, closeLog, err := openLog(logPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "zettel: opening the log failed: %v\n", err)
		return err
	}
	defer closeLog()
	log = log.With(zlog.Context{"socket": socketPath, "database": dbPath})
	defer func() {
		if err != nil {
			log.Error("zettel.exit", "zettel is exiting on a failure", zlog.Context{"cause": err.Error()})
		} else {
			log.Info("zettel.exit", "zettel is exiting cleanly", nil)
		}
	}()
	reportBuild(log)

	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, syscall.SIGINT, syscall.SIGTERM)
	defer signal.Stop(sigc)

	db, err := graph.Open(context.Background(), dbPath, log, graph.Options{})
	if err != nil {
		return err
	}
	defer func() {
		if closeErr := db.Close(); closeErr != nil {
			err = errors.Join(err, closeErr)
		}
	}()
	ln, err := server.Listen(socketPath, log)
	if err != nil {
		return err
	}
	srv := server.New(db, log)
	errc := make(chan error, 1)
	go func() { errc <- srv.Serve(ln) }()
	select {
	case sig := <-sigc:
		log.Info("zettel.shutdown", "received a termination signal", zlog.Context{"signal": sig.String()})
		ctx, cancel := context.WithTimeout(context.Background(), shutdownGrace)
		defer cancel()
		if err := srv.Shutdown(ctx); err != nil {
			log.Error("zettel.shutdown", "draining in-flight requests failed", zlog.Context{"cause": err.Error()})
			return err
		}
		if serveErr := <-errc; serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			return serveErr
		}
		return nil
	case serveErr := <-errc:
		if serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			log.Error("zettel.serve", "serving ended on a failure", zlog.Context{"cause": serveErr.Error()})
			return serveErr
		}
		return nil
	}
}

func openLog(path string) (*zlog.Logger, func(), error) {
	log, closeFile, err := recordlog.Open(zlog.Runtime, path, os.Stderr, os.Getenv, time.Now)
	if err != nil {
		return nil, nil, err
	}
	return log, func() { _ = closeFile() }, nil
}

// reportBuild writes zettel's build report so the daemon's deploy can tell
// whether it runs the fresh build (buildreport.Announce).
func reportBuild(log *zlog.Logger) {
	buildreport.Announce(buildreport.ServiceZettel, "zettel.buildreport", log, os.Getenv)
}

func cacheDir(home string) string {
	if d := os.Getenv("XDG_CACHE_HOME"); d != "" {
		return filepath.Join(d, "agent-repl")
	}
	return filepath.Join(home, ".cache", "agent-repl")
}

func envOr(name, def string) string {
	if v := os.Getenv(name); v != "" {
		return v
	}
	return def
}
