#!/bin/sh
# Publishes the landing page: commits site/ as it is in HEAD on top of
# gh-pages, which GitHub Pages serves, and pushes it. gh-pages has its
# own history, so this works where git subtree push does not.
set -eu
git fetch -q origin gh-pages
tree=$(git rev-parse HEAD:site)
if [ "$tree" = "$(git rev-parse origin/gh-pages^{tree})" ]; then
  echo "gh-pages already has site/ as it is in HEAD."
  exit 0
fi
message=${1:-"$(git log -1 --format=%s -- site)"}
commit=$(git commit-tree "$tree" -p origin/gh-pages -m "$message")
git push -q origin "$commit:refs/heads/gh-pages"
echo "Published $(git rev-parse --short HEAD)'s site/ as gh-pages $(git rev-parse --short "$commit")."
