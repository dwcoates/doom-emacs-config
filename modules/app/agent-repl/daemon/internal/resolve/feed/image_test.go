package feed

import (
	"fmt"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
)

// pathBlock is an ImageBlock naming a host file.
func pathBlock(path, mediaType string) *conversationv1.ImageBlock {
	return &conversationv1.ImageBlock{
		Location:  &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: path}},
		MediaType: mediaType,
	}
}

// urlBlock is an ImageBlock naming a fetchable url.
func urlBlock(url string) *conversationv1.ImageBlock {
	return &conversationv1.ImageBlock{
		Location: &conversationv1.ImageBlock_Url{Url: &conversationv1.ImageBlockUrl{Url: url}},
	}
}

// registrarReturning is a registrar that answers SRC, recording what it saw.
func registrarReturning(src string, seen *[]string) ImageRegistrar {
	return func(path, mediaType string) (string, error) {
		*seen = append(*seen, path+"|"+mediaType)
		return src, nil
	}
}

// TestPathImageResolverRefusesWithoutARegistrar covers the construction
// refusal: a resolver with no registrar could only invent a src.
func TestPathImageResolverRefusesWithoutARegistrar(t *testing.T) {
	// Arrange, Act.
	resolve, err := PathImageResolver(nil, dlog.NewTestLogger())

	// Assert.
	if err == nil {
		t.Fatalf("PathImageResolver built a resolver %v with no registrar, want a refusal", resolve)
	}
}

// TestPathImageResolverRefusesWithoutALogger covers the other construction
// refusal: the resolver's failures must have somewhere to be recorded.
func TestPathImageResolverRefusesWithoutALogger(t *testing.T) {
	// Arrange.
	var seen []string

	// Act.
	resolve, err := PathImageResolver(registrarReturning("/feed-images/x", &seen), nil)

	// Assert.
	if err == nil {
		t.Fatalf("PathImageResolver built a resolver %v with no logger, want a refusal", resolve)
	}
}

// TestPathArmResolvesThroughTheRegistrar covers the composer's own case: an
// attached file is made servable and drawn from the daemon's image origin.
func TestPathArmResolvesThroughTheRegistrar(t *testing.T) {
	// Arrange.
	var seen []string
	resolve, err := PathImageResolver(registrarReturning("/feed-images/abc", &seen), dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("build the resolver: %v", err)
	}

	// Act.
	src, _, err := resolve(pathBlock("/w/.claude/emacs/images/clip.png", "image/png"))

	// Assert.
	if err != nil {
		t.Fatalf("resolve a path arm: %v", err)
	}
	if src != "/feed-images/abc" {
		t.Errorf("the src is %q, want the registrar's own answer", src)
	}
	if want := []string{"/w/.claude/emacs/images/clip.png|image/png"}; strings.Join(seen, ",") != strings.Join(want, ",") {
		t.Errorf("the registrar saw %v, want %v", seen, want)
	}
}

// TestPathArmDrawsTheFileNameAsAltText covers the alt: it is the name the
// composer's own marker line drew when the user attached the file.
func TestPathArmDrawsTheFileNameAsAltText(t *testing.T) {
	// Arrange.
	var seen []string
	resolve, err := PathImageResolver(registrarReturning("/feed-images/abc", &seen), dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("build the resolver: %v", err)
	}

	// Act.
	_, alt, err := resolve(pathBlock("/w/.claude/emacs/images/clip.png", "image/png"))

	// Assert.
	if err != nil {
		t.Fatalf("resolve a path arm: %v", err)
	}
	if alt != "clip.png" {
		t.Errorf("the alt text is %q, want the file's own name", alt)
	}
}

// TestPathArmSurfacesARegistrarRefusal covers the refusal travelling out
// rather than being defaulted into an empty src.
func TestPathArmSurfacesARegistrarRefusal(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	refuse := func(string, string) (string, error) { return "", fmt.Errorf("is not absolute") }
	resolve, err := PathImageResolver(refuse, log)
	if err != nil {
		t.Fatalf("build the resolver: %v", err)
	}

	// Act.
	src, _, err := resolve(pathBlock("relative.png", "image/png"))

	// Assert.
	if err == nil {
		t.Fatalf("the resolver answered %q for a refused registration, want the refusal", src)
	}
	if !loggedAt(log, "error", "daemon.feed.image_unregistrable") {
		t.Errorf("the refused registration was not recorded; records: %v", log.Records())
	}
}

// TestURLArmIsAnsweredVerbatim covers the vendor-supplied url: the daemon has
// no reason to put itself in the middle of that fetch.
func TestURLArmIsAnsweredVerbatim(t *testing.T) {
	// Arrange.
	var seen []string
	resolve, err := PathImageResolver(registrarReturning("/feed-images/abc", &seen), dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("build the resolver: %v", err)
	}

	// Act.
	src, _, err := resolve(urlBlock("https://example.invalid/a.png"))

	// Assert.
	if err != nil {
		t.Fatalf("resolve a url arm: %v", err)
	}
	if src != "https://example.invalid/a.png" {
		t.Errorf("the src is %q, want the url verbatim", src)
	}
	if len(seen) != 0 {
		t.Errorf("the registrar was called %v for a url arm, want not at all", seen)
	}
}

// TestURLArmWithNoURLIsRefused covers the empty url: an `<img src="">`
// reloads the page's own document, which is worse than drawing nothing.
func TestURLArmWithNoURLIsRefused(t *testing.T) {
	// Arrange.
	var seen []string
	log := dlog.NewTestLogger()
	resolve, err := PathImageResolver(registrarReturning("/feed-images/abc", &seen), log)
	if err != nil {
		t.Fatalf("build the resolver: %v", err)
	}

	// Act.
	src, _, err := resolve(urlBlock(""))

	// Assert.
	if err == nil {
		t.Fatalf("the resolver answered %q for an empty url, want a refusal", src)
	}
	if !loggedAt(log, "error", "daemon.feed.image_unresolvable") {
		t.Errorf("the empty url was not recorded; records: %v", log.Records())
	}
}

// urlResolver builds a resolver whose registrar must not be reached, for the
// url-arm cases; the returned logger is what the refusals are read off.
func urlResolver(t *testing.T) (ImageResolver, *dlog.TestLogger) {
	t.Helper()
	var seen []string
	log := dlog.NewTestLogger()
	resolve, err := PathImageResolver(registrarReturning("/feed-images/abc", &seen), log)
	if err != nil {
		t.Fatalf("build the resolver: %v", err)
	}
	return resolve, log
}

// TestURLArmDrawsTheFileTheURLNamesAsAltText covers the alt a fetchable url
// carries: the file it names, the same thing the path arm's alt is.
func TestURLArmDrawsTheFileTheURLNamesAsAltText(t *testing.T) {
	// Arrange.
	resolve, _ := urlResolver(t)

	// Act.
	_, alt, err := resolve(urlBlock("https://example.invalid/shots/a.png"))

	// Assert.
	if err != nil {
		t.Fatalf("resolve a url arm: %v", err)
	}
	if alt != "a.png" {
		t.Errorf("the alt is %q, want the file the url names", alt)
	}
}

// TestADataURLCarriesNoAltText covers the inlined-bytes url: it names no file,
// and FeedImageBlock says an unnamed image carries an empty alt.
func TestADataURLCarriesNoAltText(t *testing.T) {
	// Arrange.
	resolve, _ := urlResolver(t)

	// Act.
	_, alt, err := resolve(urlBlock("data:image/png;base64,aGk="))

	// Assert.
	if err != nil {
		t.Fatalf("resolve a data url: %v", err)
	}
	if alt != "" {
		t.Errorf("the alt is %q, want the empty alt an unnamed image carries", alt)
	}
}

// TestAPathlessURLCarriesNoAltText covers a url whose path names nothing: the
// alt stays empty rather than becoming a slash or a dot.
func TestAPathlessURLCarriesNoAltText(t *testing.T) {
	// Arrange.
	resolve, _ := urlResolver(t)

	// Act.
	_, alt, err := resolve(urlBlock("https://example.invalid/"))

	// Assert.
	if err != nil {
		t.Fatalf("resolve a pathless url: %v", err)
	}
	if alt != "" {
		t.Errorf("the alt is %q, want no name at all", alt)
	}
}

// TestAnUnparseableURLIsRefused covers a url that is not one: no src is drawn
// from a reference the daemon cannot even read.
func TestAnUnparseableURLIsRefused(t *testing.T) {
	// Arrange.
	resolve, log := urlResolver(t)

	// Act.
	src, _, err := resolve(urlBlock("https://example.invalid/%zz"))

	// Assert.
	if err == nil {
		t.Fatalf("the resolver answered %q for an unparseable url, want a refusal", src)
	}
	if !loggedAt(log, "error", "daemon.feed.image_url_unparseable") {
		t.Errorf("the unparseable url was not recorded; records: %v", log.Records())
	}
}

// TestAURLSchemeAnImageMustNotBeHandedIsRefused covers the scheme allowlist:
// the daemon owns the resolution, so the set an `<img>` may be handed is
// stated here rather than sniffed by the client.
func TestAURLSchemeAnImageMustNotBeHandedIsRefused(t *testing.T) {
	// Arrange.
	resolve, log := urlResolver(t)

	// Act.
	src, _, err := resolve(urlBlock("file:///etc/passwd"))

	// Assert.
	if err == nil {
		t.Fatalf("the resolver answered %q for a file: url, want a refusal", src)
	}
	if !loggedAt(log, "error", "daemon.feed.image_url_scheme") {
		t.Errorf("the refused scheme was not recorded; records: %v", log.Records())
	}
}

// TestUnsetLocationIsRefused covers an image block with neither arm set.
func TestUnsetLocationIsRefused(t *testing.T) {
	// Arrange.
	var seen []string
	log := dlog.NewTestLogger()
	resolve, err := PathImageResolver(registrarReturning("/feed-images/abc", &seen), log)
	if err != nil {
		t.Fatalf("build the resolver: %v", err)
	}

	// Act.
	src, _, err := resolve(&conversationv1.ImageBlock{MediaType: "image/png"})

	// Assert.
	if err == nil {
		t.Fatalf("the resolver answered %q for an unset location, want a refusal", src)
	}
	if !loggedAt(log, "error", "daemon.feed.image_unresolvable") {
		t.Errorf("the unset location was not recorded; records: %v", log.Records())
	}
}

// loggedAt reports whether the logger captured a record at LEVEL under
// OPERATION.
func loggedAt(log *dlog.TestLogger, level, operation string) bool {
	for _, record := range log.Records() {
		if record.Level == level && record.Operation == operation {
			return true
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// The shared block drawer
// ---------------------------------------------------------------------------

// TestDrawUserBlocksSkipsTextTheStripEmptied covers the block whose whole text
// was a sentinel span: there is nothing to draw, and an empty text block draws
// as an empty line in the bubble.
func TestDrawUserBlocksSkipsTextTheStripEmptied(t *testing.T) {
	// Arrange.
	content := &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "all sentinel"}}},
	}}
	strip := func(string) string { return "" }

	// Act.
	blocks := DrawUserBlocks(content, strip, refusingResolver, dlog.NewTestLogger())

	// Assert.
	if len(blocks) != 0 {
		t.Fatalf("the drawn blocks are %v, want none", blocks)
	}
}

// TestDrawUserBlocksKeepsTheComposedOrder covers the order the person
// composed in: the words, then the attachment.
func TestDrawUserBlocksKeepsTheComposedOrder(t *testing.T) {
	// Arrange.
	content := &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "look"}}},
		{Block: &conversationv1.UserContentBlock_Image{Image: pathBlock("/w/clip.png", "image/png")}},
	}}
	var seen []string
	resolve, err := PathImageResolver(registrarReturning("/feed-images/abc", &seen), dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("build the resolver: %v", err)
	}

	// Act.
	blocks := DrawUserBlocks(content, func(s string) string { return s }, resolve, dlog.NewTestLogger())

	// Assert.
	if len(blocks) != 2 {
		t.Fatalf("the drawn blocks are %v, want the words then the image", blocks)
	}
	if got := blocks[0].GetText().GetText(); got != "look" {
		t.Errorf("the first block draws %q, want the words", got)
	}
	if got := blocks[1].GetImage().GetSrc(); got != "/feed-images/abc" {
		t.Errorf("the second block's src is %q, want the resolved source", got)
	}
}

// TestDrawUserBlocksNamesAnUnresolvableImage covers the refusal: the person is
// TOLD something they attached could not be drawn, rather than it vanishing.
func TestDrawUserBlocksNamesAnUnresolvableImage(t *testing.T) {
	// Arrange.
	content := &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Image{Image: pathBlock("/w/clip.png", "image/png")}},
	}}
	log := dlog.NewTestLogger()

	// Act.
	blocks := DrawUserBlocks(content, func(s string) string { return s }, refusingResolver, log)

	// Assert.
	if len(blocks) != 1 {
		t.Fatalf("the drawn blocks are %v, want the named refusal", blocks)
	}
	if got := blocks[0].GetUnsupported().GetKind(); got != "image" {
		t.Errorf("the refusal names %q, want \"image\"", got)
	}
	if !loggedAt(log, "warn", "daemon.feed.image_unresolved") {
		t.Errorf("the unresolved image was not recorded; records: %v", log.Records())
	}
}

// refusingResolver stands in for a resolver that cannot place a reference.
func refusingResolver(*conversationv1.ImageBlock) (string, string, error) {
	return "", "", fmt.Errorf("nothing resolves this reference")
}
