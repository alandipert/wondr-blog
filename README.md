# wondr-blog

This is the source code for [wondr](https://tailrecursion.com/wondr/), my blog
about R programming.

Posts are stored in Markdown and converted to HTML with Pandoc. GNU Make and
Emacs orchestrate the process.

## Development

You'll need the following software installed. Dockerfile forthcoming:

* Pandoc
* Emacs 25+
* [`sass`](https://sass-lang.com/)

Generate HTML and atom.xml in the `public` directory:

    make public
    
From there you can use a local file server to serve the public directory locally. With Python:

    cd public
    python -m SimpleHTTPServer

If you want to get fancy, you can build on file save
with [entr](http://entrproject.org/) and reload the browser automatically
with [live-server](https://github.com/tapio/live-server).

## Deployment

The website is hosted on AWS in an S3 bucket and CloudFront is used for caching.

`make deploy` deploys the website to my AWS account and only works if you're
me. To make this your own, you'll need to edit several things in the `Makefile`.

## Thanks

* To Munif Tanjim for
  the [minimo blog theme](https://github.com/MunifTanjim/minimo) I use with
  modifications.
