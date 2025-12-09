# Dockerfile

# --- Stage 1: The Builder ---
FROM haskell:9.4 AS builder
WORKDIR /app
COPY *.cabal ./
RUN cabal update
COPY . .
RUN cabal install --install-method=copy --installdir=/dist

# --- Stage 2: The Final Image ---
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    libgmp10 locales ffmpeg && \
    sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

WORKDIR /app

# Copy binary
COPY --from=builder /dist/Julia /usr/local/bin/julia

# Create a specific directory for the final export
RUN mkdir -p /export

# CMD Sequence:
# 1. Run Julia (Generates PPMs in /app/output/ - internal only)
# 2. Run FFmpeg (Reads /app/output/, writes MP4 to /export/ - mounted to host)
CMD ["sh", "-c", "julia && ffmpeg -framerate 30 -i output/frame_%04d.png -c:v libx264 -pix_fmt yuv420p -y /export/julia_movie.mp4"]