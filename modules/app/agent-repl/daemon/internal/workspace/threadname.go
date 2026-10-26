package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A CHAT THREAD'S NAME IS ITS FIRST QUESTION'S ABBREVIATION (owner ruling,
// 2026-10-26), decided by the chat service, which mints the abbreviations
// (internal/chat, naming.go), and applied here (NameThread). A thread whose
// first question is its own abbreviation is registered under it at once; any
// other is registered under ThreadTitlePlaceholder until its abbreviation
// names it. Threads that start alike share a name: nothing suffixes it.

const (
	// ThreadTitlePlaceholder is a thread's name until its first question's
	// abbreviation names it.
	ThreadTitlePlaceholder = "Loading…"
	// opThreadName is the naming's log operation.
	opThreadName = "daemon.workspace.thread_name"
)

// NameThread names thread WS NAME and republishes every surface that draws
// the name; a thread already so named is left as it is.
func (v *verbs) NameThread(ctx context.Context, ws ids.WorkspaceID, name string) error {
	v.names.mu.Lock()
	record, err := v.deps.DB.Workspace(ctx, ws)
	if err != nil {
		v.names.mu.Unlock()
		v.deps.Log.Global().Error(opThreadName, "could not read the thread to name it", dlog.Context{"workspace": string(ws), "name": name, "cause": err.Error()})
		return fmt.Errorf("name thread %s: %w", ws, err)
	}
	if !record.IsThread() {
		v.names.mu.Unlock()
		panic(fmt.Sprintf("workspace: NameThread named %s, which is no thread", ws))
	}
	log := v.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(ws)})
	if record.Name == name {
		v.names.mu.Unlock()
		log.Debug(opThreadName, "the thread already holds its name", dlog.Context{"name": name})
		return nil
	}
	err = v.deps.DB.SetName(ctx, ws, name)
	v.names.mu.Unlock()
	if err != nil {
		log.Error(opThreadName, "could not record the thread's name", dlog.Context{"name": name, "was": record.Name, "cause": err.Error()})
		return fmt.Errorf("name thread %s: %w", ws, err)
	}
	log.Info(opThreadName, "named the thread after its first question", dlog.Context{"name": name, "was": record.Name})
	v.publishRename(ctx, log, ws, name)
	return nil
}

// OpenThreads has the chat service open every thread the registry holds, so
// each is named after its first question and its questions abbreviated
// (backfilling what a stopped daemon, or one before names were
// abbreviations, left). Run at boot; the threads open on their own
// goroutine, each failure recorded by the chat service.
func (v *verbs) OpenThreads(ctx context.Context) error {
	global := v.deps.Log.Global()
	records, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		global.Error(opThreadName, "could not list the workspaces to open the threads", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("open threads: %w", err)
	}
	var threads []ids.WorkspaceID
	for _, record := range records {
		if record.IsThread() {
			threads = append(threads, record.ID)
		}
	}
	global.Info(opThreadName, "opening every thread to name it and abbreviate its questions", dlog.Context{"threads": len(threads)})
	detached := context.WithoutCancel(ctx)
	v.threadOpens.Add(1)
	go func() {
		defer v.threadOpens.Done()
		for _, id := range threads {
			// A THREAD THAT WILL NOT OPEN is recorded by the chat service,
			// which owns the read; the next one still opens.
			_ = v.deps.Threads.Open(detached, id)
		}
	}()
	return nil
}
