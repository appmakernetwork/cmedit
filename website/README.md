# CMeDit website

The marketing site and release white paper for CMeDit.

- `index.html` — the landing page
- `whitepaper.html` — *CMeDit: A Text Editor That Exists*, the official release
  white paper (Technical Report AMN-TR-2026-01)

Both pages are fully self-contained (inline CSS, no external fonts, scripts or
images), so they work offline and can be hosted anywhere static files are
served — GitHub Pages included. To preview locally, just open the files in a
browser:

```sh
xdg-open website/index.html
```

To publish on GitHub Pages, point Pages at this directory (Settings ▸ Pages ▸
Deploy from a branch, folder `/website` — GitHub offers `/docs` or `/` by
default, so either rename this directory to `docs/` or use an Actions
workflow that uploads `website/`).
