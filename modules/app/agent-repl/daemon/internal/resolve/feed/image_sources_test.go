package feed

import (
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
)

// servesOnly is an origin check answering true for exactly SRC.
func servesOnly(src string) func(string) bool {
	return func(candidate string) bool { return candidate == src }
}

// resolverAnswering is an image resolver answering SRC and ALT, or ERR.
func resolverAnswering(src, alt string, err error) ImageResolver {
	return func(*conversationv1.ImageBlock) (string, string, error) { return src, alt, err }
}

// TestNewImageSourcesRefusesAMissingDependency covers each construction
// refusal: the sources cannot resolve, check or record without their parts.
func TestNewImageSourcesRefusesAMissingDependency(t *testing.T) {
	tests := []struct {
		name    string
		resolve ImageResolver
		served  func(string) bool
		log     dlog.Logger
		want    string
	}{
		{"no resolver", nil, servesOnly(""), dlog.NewTestLogger(), "image resolver"},
		{"no origin check", resolverAnswering("", "", nil), nil, dlog.NewTestLogger(), "origin check"},
		{"no logger", resolverAnswering("", "", nil), servesOnly(""), nil, "logger"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			_, err := NewImageSources(tt.resolve, tt.served, tt.log)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Errorf("NewImageSources = %v, want a refusal naming %q", err, tt.want)
			}
		})
	}
}

// TestComposerIsThePromptsOwnResolution covers the composer arm: the overlay
// shows a composer image from the source a sent prompt's bubble will.
func TestComposerIsThePromptsOwnResolution(t *testing.T) {
	// Arrange.
	sources, err := NewImageSources(resolverAnswering("/feed-images/abc", "clip.png", nil), servesOnly(""), dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("NewImageSources: %v", err)
	}

	// Act.
	got, err := sources.Composer(pathBlock("/w/clip.png", "image/png"))

	// Assert.
	if err != nil || got.GetSrc() != "/feed-images/abc" || got.GetAlt() != "clip.png" {
		t.Errorf("Composer = %v, %v; want the resolver's src and alt", got, err)
	}
}

// TestComposerRefusesWhatTheResolverRefuses covers the composer arm's
// refusal: an image the feed could not draw is not opened either.
func TestComposerRefusesWhatTheResolverRefuses(t *testing.T) {
	// Arrange.
	refusal := errors.New("feed: an image block states no location")
	sources, err := NewImageSources(resolverAnswering("", "", refusal), servesOnly(""), dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("NewImageSources: %v", err)
	}

	// Act.
	_, err = sources.Composer(&conversationv1.ImageBlock{})

	// Assert.
	if !errors.Is(err, refusal) {
		t.Errorf("Composer = %v, want the resolver's refusal", err)
	}
}

// TestDrawn covers which echoed sources the overlay may open: the origin's own
// and urls the feed would draw, never anything else.
func TestDrawn(t *testing.T) {
	tests := []struct {
		name   string
		src    string
		wantOK bool
	}{
		{"a source the origin serves", "/feed-images/abc", true},
		{"an https url", "https://example.invalid/a.png", true},
		{"a data url", "data:image/png;base64,AAAA", true},
		{"a route-shaped source nobody registered", "/feed-images/nope", false},
		{"a javascript url", "javascript:alert(1)", false},
		{"a file url", "file:///etc/passwd", false},
		{"the empty source", "", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			sources, err := NewImageSources(resolverAnswering("", "", nil), servesOnly("/feed-images/abc"), dlog.NewTestLogger())
			if err != nil {
				t.Fatalf("NewImageSources: %v", err)
			}

			// Act.
			err = sources.Drawn(&frontendv1.FeedImageBlock{Src: tt.src})

			// Assert.
			if (err == nil) != tt.wantOK {
				t.Errorf("Drawn(%q) = %v, want ok=%v", tt.src, err, tt.wantOK)
			}
		})
	}
}

// TestDrawnRecordsARefusedScheme covers the refusal's record: a client
// echoing a source the feed would never draw is a defect, recorded at error.
func TestDrawnRecordsARefusedScheme(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	sources, err := NewImageSources(resolverAnswering("", "", nil), servesOnly(""), log)
	if err != nil {
		t.Fatalf("NewImageSources: %v", err)
	}

	// Act.
	_ = sources.Drawn(&frontendv1.FeedImageBlock{Src: "file:///etc/passwd"})

	// Assert.
	found := false
	for _, r := range log.Records() {
		if r.Level == "error" && r.Operation == "daemon.feed.image_url_scheme" {
			found = true
		}
	}
	if !found {
		t.Errorf("the refused scheme was not recorded at error; records: %v", log.Records())
	}
}
