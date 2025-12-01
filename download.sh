#! /usr/bin/env bash
#
# see: https://github.com/GreenLightning/advent-of-code-downloader

# Store the initial state in a variable
before_files=$(find resources -type f -name "day*.txt" | sort)

# Run the downloader
~/go/bin/aocdl -output "resources/day{{.Day}}.txt" "$@"

# Store the after state in a variable
after_files=$(find resources -type f -name "day*.txt" | sort)

# Find the difference using process substitution
new_file=$(comm -13 <(echo "$before_files") <(echo "$after_files"))

if [ ! -n "$new_file" ]; then
    echo "No new file was created"
    exit 1
fi

echo "Downloaded: $new_file"

# Extract the key (e.g., "day7")
key=$(basename "$new_file" .txt)

# Get the project directory name
dir=$(ls src)

# Define filenames
sample_filename="resources/${key}-sample.txt"
clj_filename="src/${dir}/${key}.clj"

# Check if files already exist
if [ -f "$sample_filename" ]; then
    echo "Warning: Sample file $sample_filename already exists, skipping creation"
else
    touch "$sample_filename"
    echo "Created: $sample_filename"
fi

if [ -f "$clj_filename" ]; then
    echo "Warning: Clojure file $clj_filename already exists, skipping creation"
else
    # Create directory if it doesn't exist
    mkdir -p "$(dirname "$clj_filename")"

    # Create the Clojure file
    cat > "$clj_filename" << EOF
(ns ${dir}.${key}
  (:require [aoc2025.core :as aoc]))
EOF
    echo "Created: $clj_filename"
fi

echo "Setup complete: $clj_filename $sample_filename"
