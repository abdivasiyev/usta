# The landing page

The page at <https://usta.azizovich.uz> is `site/` on `master`. GitHub
Pages serves it from the `gh-pages` branch, a copy of `site/`:
`site/CNAME` names the domain, and `site/.nojekyll` keeps the files as
they are.

## Publishing a change

Commit the change to `site/` on `master` and push it, then copy `site/`
to `gh-pages`:

```sh
./scripts/publish-site.sh
```

It commits `site/` as it is in `HEAD` on top of `gh-pages`. `git subtree
push` does not work here: `gh-pages` has a history of its own.

The page is live a minute or so later.

If the site answers 404 after Pages' settings changed (its source branch
or folder), Pages may still serve its last build from the old source.
Ask for a new build of `gh-pages`:

```sh
gh api -X POST repos/abdivasiyev/usta/pages/builds
```

## The download links

The download buttons go to a release's own files,
`https://github.com/abdivasiyev/usta/releases/download/<tag>/<file>`.
The release workflow keeps a new release a draft until all its files
are attached, then publishes it, points these links at it, commits that
to `master` and publishes the page with `scripts/publish-site.sh`: a
release needs no change here by hand. The page reads the newest release
from GitHub's API for its number and the `.deb` and `.rpm`, whose names
carry the version.

## The domain

`usta.azizovich.uz` is a CNAME to `abdivasiyev.github.io` in
Cloudflare, DNS only: proxied, GitHub cannot check the domain or renew
its HTTPS certificate.
