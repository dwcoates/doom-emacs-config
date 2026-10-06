package main

import (
	"fmt"
	"path/filepath"
	"time"

	"claude-repld/internal/clock"
	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/newsdigest"
	"claude-repld/internal/wsm"
)

// The news digest's TEST knobs: they compress its schedule (Go durations;
// unset is the production window). A malformed or non-positive value is a
// BOOT REFUSAL. newsdigest.EnvSources (a sources file replacing the real
// sources) is the third.
const (
	envNewsDigestStartDelay = "AGENT_REPL_NEWS_DIGEST_START_DELAY"
	envNewsDigestEvery      = "AGENT_REPL_NEWS_DIGEST_EVERY"
	envNewsDigestRecheck    = "AGENT_REPL_NEWS_DIGEST_RECHECK"
)

// newsDigestLockName is a digest run's cross-process lock, in the kernel-lock
// directory, so an incumbent and its handover successor never run together.
const newsDigestLockName = "news-digest.lock"

// newsDigestInputs are what buildNewsDigest is built from.
type newsDigestInputs struct {
	// Guard is the vendor guard the REAL sources' fetches ask.
	Guard newsdigest.Guard
	// Headless runs the condensing call.
	Headless headless.Runner
	// PromptsDir holds the brief.
	PromptsDir string
	// ConfigDir is the account the condensing call bills.
	ConfigDir string
	// Store is the state client.
	Store newsdigest.Store
	// RunDir is the kernel-lock directory.
	RunDir string
	// Serves answers whether this daemon serves.
	Serves func() bool
	// SDKVersion answers the Agent SDK version agent-repl runs.
	SDKVersion func() (string, bool)
	// Getenv reads the knobs.
	Getenv func(string) string
	// Log is the global logger.
	Log dlog.Logger
}

// buildNewsDigest builds the daily news digest from its knobs. A refused knob
// or sources file is a BOOT FATAL, recorded here.
func buildNewsDigest(in newsDigestInputs) (*newsdigest.Digester, error) {
	var (
		start, every, recheck time.Duration
		err                   error
	)
	if start, err = resolveDurationKnob(envNewsDigestStartDelay, in.Getenv(envNewsDigestStartDelay), newsdigest.DefaultStartDelay); err != nil {
		return nil, refusedNewsDigestKnob(in.Log, err)
	}
	if every, err = resolveDurationKnob(envNewsDigestEvery, in.Getenv(envNewsDigestEvery), newsdigest.DefaultEvery); err != nil {
		return nil, refusedNewsDigestKnob(in.Log, err)
	}
	if recheck, err = resolveDurationKnob(envNewsDigestRecheck, in.Getenv(envNewsDigestRecheck), newsdigest.DefaultRecheck); err != nil {
		return nil, refusedNewsDigestKnob(in.Log, err)
	}
	// THE REAL SOURCES ARE GUARDED, a named sources file is not: the same rule
	// as the headless binary, whose guard refuses only the default `claude`.
	sources := newsdigest.DefaultSources
	var fetcher newsdigest.Fetcher = newsdigest.GuardedFetcher{Guard: in.Guard, Inner: newsdigest.HTTPFetcher{}}
	sourcesFile := in.Getenv(newsdigest.EnvSources)
	if sourcesFile != "" {
		if sources, err = newsdigest.LoadSources(sourcesFile); err != nil {
			return nil, refusedNewsDigestKnob(in.Log, err)
		}
		fetcher = newsdigest.HTTPFetcher{}
	}
	digest, err := newsdigest.New(newsdigest.Deps{
		Sources:    sources,
		Fetcher:    fetcher,
		Headless:   in.Headless,
		PromptsDir: in.PromptsDir,
		ConfigDir:  in.ConfigDir,
		Store:      in.Store,
		Clock:      clock.System{},
		LockPath:   filepath.Join(in.RunDir, newsDigestLockName),
		Serves:     in.Serves,
		SDKVersion: in.SDKVersion,
		MintID:     wsm.NewNewsDigestID,
		Every:      every,
		StartDelay: start,
		Recheck:    recheck,
		Log:        in.Log,
	})
	if err != nil {
		return nil, fmt.Errorf("claude-repld: build the news digest: %w", err)
	}
	in.Log.Debug(graphOperation, "the news digest is built", dlog.Context{
		"sources": len(sources), "sources_file": sourcesFile,
		"start_delay": start.String(), "every": every.String(), "recheck": recheck.String(),
	})
	return digest, nil
}

// refusedNewsDigestKnob records a refused news digest knob.
func refusedNewsDigestKnob(log dlog.Logger, err error) error {
	log.Error(graphOperation, "a news digest knob was refused", dlog.Context{"cause": err.Error()})
	return err
}
