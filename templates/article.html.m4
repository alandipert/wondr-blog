<html>
  <head>
    <title>wondr</title>
    <link rel="stylesheet" href="../style.css"/>
  </head>
  <body class="page">
    <div class="site">
      <header id="header" class="header-container">
        <div class="site-header">
          <nav id="navmenu" aria-label="Main Menu">
            <ul class="main-menu">
              <li>
                <a href="/" aria-current="page">Home</a>
              </li>
              <li>
                <a href="/about/">About</a>
              </li>
            </ul>
          </nav>
          <div class="site-info">
            <h1 class="site-title title">Wondr</h1>
            <p class="site-description">Wondering about R</p>
          </div>
        </div>
      </header>
      <main class="main">
        <article lang="en" class="entry">
          <header class="entry-header">
            <div class="entry-info">
              <h1 class="entry-title title">$title$</h1>
            </div>
            <div class="meta">
              <span class="posted-on">
                <svg class="icon" viewBox="0 0 24 24" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" aria-hidden="true">
                  <rect x="3" y="4" width="18" height="18" rx="2" ry="2"></rect>
                  <line x1="16" y1="2" x2="16" y2="6"></line>
                  <line x1="8" y1="2" x2="8" y2="6"></line>
                  <line x1="3" y1="10" x2="21" y2="10"></line>
                </svg>
                <span class="screen-reader">Posted on </span>
                <time class="date" datetime="$iso-date$">$pretty-date$</time>
              </span>
              <span class="reading-time">
                <svg class="icon" viewBox="0 0 24 24" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" aria-hidden="true">
                  <circle cx="12" cy="12" r="10"></circle>
                  <polyline points="12 6 12 12 15 15"></polyline>
                </svg>
                $reading-time$
              </span>
            </div>
          </header>
          <div class="entry-content">
            $body$
          </div>
        </article>
        <article data-sblg-article="1" class="entry-content"></article>
      </main>
      <footer id="footer" class="footer-container">
        <div class="footer">
          <div class="copyright">
            <p>© $footer-year$ Alan Dipert</p>
          </div>
        </div>
      </footer>
    </div>
  </body>
</html>
