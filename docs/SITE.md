# The landing page

The page at <https://usta.azizovich.uz> is `site/` on `master`. GitHub
Pages serves it from the `gh-pages` branch, a copy of `site/`:
`site/CNAME` names the domain, and `site/.nojekyll` keeps the files as
they are.

## Publishing a change

Commit the change to `site/` on `master`, then copy `site/` to
`gh-pages`:

```sh
git subtree push --prefix site origin gh-pages
```

The page is live a minute or so later.

If the site answers 404 after Pages' settings changed (its source branch
or folder), Pages may still serve its last build from the old source.
Ask for a new build of `gh-pages`:

```sh
gh api -X POST repos/abdivasiyev/usta/pages/builds
```

## The download links

The download buttons go to
`https://github.com/abdivasiyev/usta/releases/latest/download/<file>`,
which GitHub sends to the newest release's `usta.dmg` or
`usta-x86_64.AppImage`, so a release needs no change here. The page
reads the newest release from GitHub's API for its number and the
`.deb` and `.rpm`, whose names carry the version.

## The domain

`usta.azizovich.uz` is a CNAME to `abdivasiyev.github.io` in
Cloudflare, DNS only: proxied, GitHub cannot check the domain or renew
its HTTPS certificate.
