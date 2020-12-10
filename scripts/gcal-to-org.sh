#!/usr/bin/env bash
set -euo pipefail

# Downloads ics files from google calendar and converts them to org files,
# which are saved in my agenda directory.
#
# Depends on:
# - https://github.com/asoroa/ical2org.py
# - gcal-to-org-urls.sh (should also be in scripts dir): this script should
#   create a dictionary named calendars, which maps the calendar name (which
#   will be the name of the resulting org file, minus the org extension) to the
#   ics file url.

source gcal-to-org-urls.sh

for cal_name in "${!calendars[@]}"; do
    url=${calendars[$cal_name]}
    icsfile="$cal_name.ics"
    curl "$url" -o "$icsfile"
    ical2orgpy "$icsfile" ~/Dropbox/Org/agenda/"$cal_name.org"
    rm "$icsfile"
done
