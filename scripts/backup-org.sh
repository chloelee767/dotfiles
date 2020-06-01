#!/usr/bin/env bash
cd ~/Documents/org
git add -u # stage deleted files
git add * # stage everything else
git commit -m "Automated backup"
git push
