package session

import (
	"encoding/json"
	"testing"
)

// The real shapes, as observed in live CLI transcripts.
const (
	asyncAgentSpawn = `{"isAsync":true,"status":"async_launched","agentId":"ad2e2275ec191e1e1",` +
		`"description":"Find engine spawning logic","canReadOutputFile":true,` +
		`"outputFile":"/tmp/claude-501/slug/sess/tasks/ad2e2275ec191e1e1.output"}`
	syncAgentResult = `{"agentId":"a3d58ca2fbd85e3be","agentType":"general-purpose","status":"completed",` +
		`"totalTokens":24368,"totalToolUseCount":4}`
	bgShellResult = `{"stdout":"","stderr":"","interrupted":false,"isImage":false,` +
		`"noOutputExpected":false,"backgroundTaskId":"bf8vj85v1"}`
	plainShellResult = `{"stdout":"hi","stderr":"","interrupted":false,"isImage":false,"noOutputExpected":false}`
)

func TestClassifyAsyncSourceNamesABackgroundAgentBySourceID(t *testing.T) {
	// Arrange / Act
	src := classifyAsyncSource("Agent", json.RawMessage(asyncAgentSpawn), false)
	// Assert
	if src == nil || src.SourceID != "ad2e2275ec191e1e1" {
		t.Fatalf("want the announced agentId as the source id, got %+v", src)
	}
}

func TestClassifyAsyncSourceGivesABackgroundAgentATranscriptStream(t *testing.T) {
	// Arrange / Act
	src := classifyAsyncSource("Agent", json.RawMessage(asyncAgentSpawn), false)
	// Assert — a transcript renders as nested bubbles, which is the whole point.
	if src.Stream == nil || src.Stream.Format != "jsonl-transcript" || src.Stream.Transport != "poll" {
		t.Fatalf("want a polled jsonl-transcript stream, got %+v", src.Stream)
	}
}

func TestClassifyAsyncSourceRefusesASynchronousSubagent(t *testing.T) {
	// Arrange / Act — a settled inline subagent, whose stream already
	// arrived parent_tool_use_id-tagged and is nested by the partition.
	src := classifyAsyncSource("Agent", json.RawMessage(syncAgentResult), false)
	// Assert
	if src != nil {
		t.Fatalf("a synchronous subagent owns no detached stream, got %+v", src)
	}
}

func TestClassifyAsyncSourceCarriesTheReadableOutputFile(t *testing.T) {
	// Arrange / Act
	src := classifyAsyncSource("Agent", json.RawMessage(asyncAgentSpawn), false)
	// Assert — the structured path is what keeps the poll route's record
	// alive when the prose announcement's wording drifts.
	if src.OutputFile != "/tmp/claude-501/slug/sess/tasks/ad2e2275ec191e1e1.output" {
		t.Fatalf("want the structured outputFile on the source, got %q", src.OutputFile)
	}
}

func TestClassifyAsyncSourceWithholdsTheOutputFileWhenUnreadable(t *testing.T) {
	// Arrange — canReadOutputFile:false forbids serving the path.
	payload := `{"isAsync":true,"agentId":"a1","outputFile":"/tmp/claude-1/s/tasks/a1.output","canReadOutputFile":false}`
	// Act
	src := classifyAsyncSource("Agent", json.RawMessage(payload), false)
	// Assert
	if src.OutputFile != "" {
		t.Fatalf("want no output file on an unreadable source, got %q", src.OutputFile)
	}
}

func TestClassifyAsyncSourceWithholdsAStreamWhenTheOutputFileIsUnreadable(t *testing.T) {
	// Arrange
	payload := `{"isAsync":true,"agentId":"a1","outputFile":"/tmp/claude-1/s/tasks/a1.output","canReadOutputFile":false}`
	// Act
	src := classifyAsyncSource("Agent", json.RawMessage(payload), false)
	// Assert — the source is still real, but nothing may be read from it.
	if src == nil || src.Stream != nil {
		t.Fatalf("want a source with no stream, got %+v", src)
	}
}

func TestClassifyAsyncSourceNamesABackgroundedShellByItsStructuredID(t *testing.T) {
	// Arrange / Act
	src := classifyAsyncSource("Bash", json.RawMessage(bgShellResult), false)
	// Assert
	if src == nil || src.SourceID != "bf8vj85v1" || src.Kind != "shell" {
		t.Fatalf("want the backgroundTaskId as a shell source, got %+v", src)
	}
}

func TestClassifyAsyncSourceGivesABackgroundedShellATextStream(t *testing.T) {
	// Arrange / Act
	src := classifyAsyncSource("Bash", json.RawMessage(bgShellResult), false)
	// Assert — a spool file is bytes; there is no structure to recover.
	if src.Stream == nil || src.Stream.Format != "text" || src.Stream.Transport != "ws" {
		t.Fatalf("want a ws-pushed text stream, got %+v", src.Stream)
	}
}

func TestClassifyAsyncSourceRefusesAForegroundShell(t *testing.T) {
	// Arrange / Act
	src := classifyAsyncSource("Bash", json.RawMessage(plainShellResult), false)
	// Assert
	if src != nil {
		t.Fatalf("a foreground shell owns no stream, got %+v", src)
	}
}

func TestClassifyAsyncSourceRefusesAFailedSpawn(t *testing.T) {
	// Arrange / Act — an errored call announces no work to stream, whatever
	// its payload happens to say.
	src := classifyAsyncSource("Agent", json.RawMessage(asyncAgentSpawn), true)
	// Assert
	if src != nil {
		t.Fatalf("a failed spawn owns no stream, got %+v", src)
	}
}

func TestClassifyAsyncSourceRefusesAnAbsentStructuredResult(t *testing.T) {
	// Arrange / Act — a shim predating §1.2 `structured`, or a tool the SDK
	// gave no structured result for.
	src := classifyAsyncSource("Agent", nil, false)
	// Assert
	if src != nil {
		t.Fatalf("want no source without a structured result, got %+v", src)
	}
}

func TestClassifyAsyncSourceRefusesANonObjectStructuredResult(t *testing.T) {
	// Arrange / Act — some tools report a bare string result.
	src := classifyAsyncSource("Agent", json.RawMessage(`"just a string"`), false)
	// Assert
	if src != nil {
		t.Fatalf("a bare string result is not a spawn, got %+v", src)
	}
}

func TestClassifyAsyncSourceReadsAnAsyncLaunchAsRunning(t *testing.T) {
	// Arrange / Act
	src := classifyAsyncSource("Agent", json.RawMessage(asyncAgentSpawn), false)
	// Assert
	if src.Status != "running" {
		t.Fatalf("want async_launched to read as running, got %q", src.Status)
	}
}

func TestClassifyAsyncSourceMapsAKilledStatusOntoTheClosedEnum(t *testing.T) {
	// Arrange
	payload := `{"isAsync":true,"agentId":"a1","status":"killed"}`
	// Act
	src := classifyAsyncSource("Agent", json.RawMessage(payload), false)
	// Assert
	if src.Status != "killed" {
		t.Fatalf("want killed, got %q", src.Status)
	}
}

func TestClassifyAsyncSourceReadsAnUnknownStatusAsRunningRatherThanTerminal(t *testing.T) {
	// Arrange — a status the harness added after this daemon shipped.
	payload := `{"isAsync":true,"agentId":"a1","status":"reticulating_splines"}`
	// Act
	src := classifyAsyncSource("Agent", json.RawMessage(payload), false)
	// Assert — a wrong "done" hides live output; a wrong "running" only
	// spins a beat too long, so unknown must fail toward running.
	if src.Status != "running" {
		t.Fatalf("want an unknown status to read as running, got %q", src.Status)
	}
}

func TestClassifyAsyncSourceRefusesAToolThatOwnsNoStream(t *testing.T) {
	// Arrange / Act
	src := classifyAsyncSource("Read", json.RawMessage(`{"file":{},"type":"text"}`), false)
	// Assert
	if src != nil {
		t.Fatalf("Read owns no stream, got %+v", src)
	}
}
