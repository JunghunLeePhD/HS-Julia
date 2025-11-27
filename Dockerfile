# Dockerfile

# --- Stage 1: The Builder ---
# Use an official Haskell image that comes with GHC and Cabal
FROM haskell:9.4 AS builder

# Set the working directory
WORKDIR /app

# Copy the cabal file and update dependencies first to leverage Docker's layer caching
COPY *.cabal ./
RUN cabal update

# Copy the rest of the application source code
COPY . .

# Build the project. The executable will be placed in a predictable directory.
RUN cabal install --install-method=copy --installdir=/dist


# --- Stage 2: The Final Image ---
# Start from a minimal base image
FROM debian:bookworm-slim

# Install the GNU Multiple Precision Arithmetic Library (runtime dependency)
# AND install and configure locales to support UTF-8 for special characters
RUN apt-get update && apt-get install -y libgmp10 locales && \
    sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    rm -rf /var/lib/apt/lists/*

# Set the locale environment variables for the container
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

# Set a working directory
WORKDIR /root/

# Copy the compiled binary from the 'builder' stage
COPY --from=builder /dist/Julia /usr/local/bin/julia

# Set the command to run when the container starts
CMD ["julia"]