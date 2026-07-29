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
  agentshim/frontend/v1/frontend.proto

.PHONY: all go ts lint test-check-generated check-generated validate coverage clean

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

# Syntax-check every proto without emitting code.
lint:
	protoc -I . --descriptor_set_out=/dev/null $(PROTOS)

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
validate: lint test-check-generated check-generated

# Generated stubs are dependencies, not coverage subjects.  Each consumer's
# own coverage configuration scopes measurement to handwritten package source.
coverage: validate
	../bin/report-nonlisp-coverage.sh daemon sidecar store wire webapp shim

clean:
	rm -rf gen
