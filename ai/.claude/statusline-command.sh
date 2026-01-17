#!/bin/bash
input=$(cat)

# Extract model info
model_name=$(echo "$input" | jq -r '.model.display_name // "Unknown"')

# Extract context window data
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
total_input=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
total_output=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')
context_size=$(echo "$input" | jq -r '.context_window.context_window_size // 0')

# Calculate total tokens used
total_used=$((total_input + total_output))

# Display format: Model | X% used (A / B tokens)
if [ -n "$used_pct" ]; then
  printf "%s | %.0f%% used (%d / %d tokens)" "$model_name" "$used_pct" "$total_used" "$context_size"
else
  printf "%s" "$model_name"
fi
