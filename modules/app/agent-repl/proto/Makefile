# Codegen for the agent-shim protobuf protocol.
#
# Outputs:
#   Go  → gen/go/...   (consumed by daemon/, agent-shim/shim-store/, agent-shim/claude/shim-sidecar/)
#   TS  → gen/ts/...   (consumed by agent-shim/claude/shim/, webapp/)
#
# Requires: protoc, protoc-gen-go, and @bufbuild/protoc-gen-es (via npx).

PROTOS := \
  agentshim/core/v1/core.proto \
  agentshim/data/v1/unknown.proto \
  agentshim/data/v1/tools.proto \
  agentshim/data/v1/stream.proto \
  agentshim/data/v1/transcript.proto \
  agentshim/data/v1/journal.proto \
  agentshim/frontend/v1/agent-emission.proto \
  agentshim/frontend/v1/async-bubble.proto \
  agentshim/frontend/v1/durable.proto \
  agentshim/frontend/v1/errors.proto \
  agentshim/frontend/v1/failure-card.proto \
  agentshim/frontend/v1/feed.proto \
  agentshim/frontend/v1/footer.proto \
  agentshim/frontend/v1/frame.proto \
  agentshim/frontend/v1/gate-revival.proto \
  agentshim/frontend/v1/host.proto \
  agentshim/frontend/v1/lifecycle.proto \
  agentshim/frontend/v1/merge.proto \
  agentshim/frontend/v1/permission-card.proto \
  agentshim/frontend/v1/prompt-queue.proto \
  agentshim/frontend/v1/response-bubble.proto \
  agentshim/frontend/v1/sidebar.proto \
  agentshim/frontend/v1/slash-menu.proto \
  agentshim/frontend/v1/state.proto \
  agentshim/frontend/v1/tokens-menu.proto \
  agentshim/frontend/v1/tokens.proto \
  agentshim/frontend/v1/tool-call.proto \
  agentshim/frontend/v1/topbar.proto \
  agentshim/frontend/v1/user-bubble.proto

.PHONY: all go ts lint durable-isolation test-check-durable-isolation test-check-generated check-generated validate coverage clean

all: go ts

go:
	mkdir -p gen/go
	protoc -I . --go_out=gen/go --go_opt=paths=source_relative $(PROTOS)

ts:
	mkdir -p gen/ts
	npx --yes @bufbuild/protoc-gen-es --version >/dev/null
	protoc -I . \
	  --plugin=protoc-gen-es=$$(npx --yes --package=@bufbuild/protoc-gen-es which protoc-gen-es 2>/dev/null || echo ./node_modules/.bin/protoc-gen-es) \
	  --es_out=gen/ts --es_opt=target=ts $(PROTOS)

# Syntax-check every proto without emitting code, then enforce the structural
# invariants the syntax check cannot see.
lint: durable-isolation
	protoc -I . --descriptor_set_out=/dev/null $(PROTOS)

# INVARIANT I6. durable.proto is the persistence evidence layer and no other
# component schema may import it or name a type it declares. This hangs off
# lint, and therefore off validate, so codegen REFUSES a schema that has already
# drifted rather than emitting bindings for the coupling and leaving the refusal
# to review.
durable-isolation:
	./check-durable-isolation.sh

test-check-durable-isolation:
	./test-check-durable-isolation.sh

# Regenerate and compare with the exact pre-generation bytes. Comparing with
# HEAD rejects a correct uncommitted schema edit, which makes the pre-commit
# validation gate impossible to satisfy.
check-generated:
	test -d gen
	@before="$$(mktemp -d "$${TMPDIR:-/tmp}/agent-repl-proto-gen.XXXXXX")"; \
	trap 'rm -rf "$$before"' EXIT; \
	cp -R gen/. "$$before"/; \
	$(MAKE) all; \
	diff -ru "$$before" gen

test-check-generated:
	./test-check-generated.sh

# Validate both the source schema and the checked-in Go/TS generated artifacts.
validate: lint test-check-durable-isolation test-check-generated check-generated

# Generated stubs are dependencies, not coverage subjects.  Each consumer's
# own coverage configuration scopes measurement to handwritten package source.
coverage: validate
	../bin/report-nonlisp-coverage.sh daemon sidecar store wire webapp shim

clean:
	rm -rf gen
