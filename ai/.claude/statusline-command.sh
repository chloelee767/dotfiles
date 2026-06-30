#!/bin/bash
input=$(cat)

# Extract model info
model_name=$(echo "$input" | jq -r '.model.display_name // "Unknown"')

session_id=$(echo "$input" | jq -r '.session_id // empty')

# Extract context window data
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
total_input=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
total_output=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')
context_size=$(echo "$input" | jq -r '.context_window.context_window_size // 0')

# Calculate total tokens used
total_used=$((total_input + total_output))

# Display format: Model | X% used (A / B tokens)
session_suffix=""
[ -n "$session_id" ] && session_suffix=" | sess: ${session_id}"

if [ -n "$used_pct" ]; then
  printf "%s | %.0f%% used (%d / %d tokens)%s" "$model_name" "$used_pct" "$total_used" "$context_size" "$session_suffix"
else
  printf "%s%s" "$model_name" "$session_suffix"
fi
