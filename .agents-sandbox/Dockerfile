FROM ghcr.io/catthehacker/ubuntu:act-22.04

ENV DEBIAN_FRONTEND=noninteractive

# Signal to claude-repl's host-only scripts (e.g. .claude/install.sh, doctor
# checks) that we are running inside the agent sandbox and they should
# no-op rather than touch the host-flavored ~/.claude/ layout.
ENV DOOM_SANDBOX=1

RUN apt-get update && apt-get install -y \
    ca-certificates curl git ripgrep uuid-runtime emacs-nox \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    rm -rf /var/lib/apt/lists/*

RUN npm install -g @anthropic-ai/claude-code
RUN npm install -g @openai/codex

RUN useradd -m -u 1000 -s /bin/bash claude
RUN git config --global --add safe.directory '*' && \
    git config --system --add safe.directory '*'

USER claude
WORKDIR /home/claude
