# wondr-blog

This is the source code for [wondr](https://tailrecursion.com/wondr/), my blog
about R programming.

Posts are stored in Markdown and converted to HTML with Pandoc. GNU Make and
Emacs orchestrate the process.

## Development

You'll need the following software installed. Dockerfile forthcoming:

* Pandoc
* Emacs 25+
* The `scss` Ruby gem

Generate HTML and atom.xml in the `public` directory:

    make public
    
From there you can use a local file server to serve the public directory locally. With Python:

    cd public
    python -m SimpleHTTPServer

If you want to get fancy, you can build on file save
with [entr](http://entrproject.org/) and reload the browser automatically
with [live-server](https://github.com/tapio/live-server).

## Deployment

Needs:

* The `s3_website` Ruby gem

    make deploy
    
This part will only work if you're me, but hopefully it's clear enough what to
edit to make this your own. The `Makefile` and `s3_website.yml` files contain
most of what's specific to me.

## Thanks

* To Munif Tanjim for
  the [minimo blog theme](https://github.com/MunifTanjim/minimo) I use with
  modifications.
