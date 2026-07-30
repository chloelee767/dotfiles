work:
    ./sync-dir.sh general shell hammerspoon work kubernetes doom neovim ai ghostty zellij lazyvim

test:
    python3 -m unittest discover tests

test-work-python:
    python3 -m unittest discover tests/work/python

bootstrap-repo:
    git config filter.agentsview-secrets.clean './scripts/strip-agentsview-secrets'
    git config filter.agentsview-secrets.smudge cat
