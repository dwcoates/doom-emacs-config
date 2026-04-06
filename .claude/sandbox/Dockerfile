FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    ca-certificates curl git ripgrep uuid-runtime \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    rm -rf /var/lib/apt/lists/*

RUN npm install -g @anthropic-ai/claude-code

RUN useradd -m -u 1000 -s /bin/bash claude
RUN git config --global --add safe.directory '*' && \
    git config --system --add safe.directory '*'

USER claude
WORKDIR /home/claude
