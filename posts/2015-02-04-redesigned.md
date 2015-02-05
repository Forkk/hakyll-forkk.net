---
title: Site Redesigned
author: Forkk
published: 2015-02-04 23:19:00
tags: meta
---

So I decided to play around with some CSS and ended up rewriting my website
again.

This new iteration of the site is now using a static site generator called
[Hakyll](http://jaspervdj.be/hakyll/), which, as you've probably guessed, is
written in Haskell. Hakyll is really nice because it is configured quite
similarly to XMonad, making it very easily scriptable and very flexible.

In other news, I've moved away from UI Kit and decided to roll my own CSS.
Furthermore, I went a bit crazy with the new semantic tags in HTML 5. This
entire page contains fewer than ten `div` tags and almost no CSS classes.
Everything is styled based on element names. This is nice because I can
completely change how the page looks through CSS alone---without touching any
HTML. I'm using CSS how the gods (W3C) intended for it to be used.

The layout is also responsive. It works really nicely on mobile devices. The nav
bar simply turns in to a few lists at the top of the page on any device with a
small screen.

Anyway, that's enough blabbering for now. Maybe I'll post something more
interesting later. Or maybe I'll continue to never post anything on this blog
until I redesign it. Who knows what will happen?
