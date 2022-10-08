#!/usr/bin/env bash
cd ~/Documents/Org
git add -u # stage deleted files
git add * # stage everything else
git commit -m "Automated backup"
git push
