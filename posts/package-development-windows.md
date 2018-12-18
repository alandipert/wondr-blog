---
date: "2018-12-18"
keywords: ["windows", "packages"]
title: "R Package Development on Windows"
id: "urn:uuid:44e90bd4-8702-42a8-b5ce-1c72ae85e46d"
abstract: |
  Windows 10 can be a productive platform for developing R applications 
  and packages. I show how to install the necessary tools and configure them.
---

Windows 10 is a totally capable platform for developing R applications and
packages and collaborating with others on [Github](https://github.com/). You
just need to install and configure the right tools.

# R

First (duh!) you need to install R. Download an installer from [the Windows
downloads page](https://cran.r-project.org/bin/windows/base/). If you've already
installed R but are not on the latest released version, it's not a bad idea to
uninstall what you have and install the latest.

# RTools

Download the version of [RTools](https://cran.r-project.org/bin/windows/Rtools/)
corresponding to the version of R you just installed and run the installer.

The default selections are all fine, except one: **make sure the checkbox
labeled "Add rtools to system PATH" is checked when you see it.**

If you don't select this option, you will have problems building R packages in
RStudio later.

# RStudio

The RStudio IDE works nicely on Windows and can be [downoloaded for
free](https://www.rstudio.com/products/rstudio/download/).

# Chocolatey

[Chocolatey](https://chocolatey.org/) is a package manager for Windows that
facilitates installing and upgrading free software like `git` and `ssh`, which
are two tools I suggest installing.

To install it, just [follow the instructions](https://chocolatey.org/install).

# `git`

Now that you've installed Chocolatey, installing `git` is a cinch. In an *Administrative* PowerShell window, run the following commands:

```
choco install git
```

# `ssh` (optional)

On Github, you have the option of cloning repositories using either [HTTPS or SSH](https://help.github.com/articles/which-remote-url-should-i-use/). I personally prefer SSH, because I clone repos all the time and I don't like to be constantly prompted for my username and password.

If you're new to `git` and Github, I suggest skipping this step. Otherwise, you can install `ssh` with `choco install openssh`.

# Configure `git`

Windows uses a different character than Mac or Linux to represent the end of a
line — the character inserted when you press *Enter* — which can make it
difficult to collaborate on text files with users on a different platform.

For those on Windows, `git` can be configured to automatically to handle this difference by running the following command in a (non-Administrator) PowerShell:

```
git config --global core.autocrlf true
```

For details on this setting, check out the [Github documentation on the subject](https://help.github.com/articles/dealing-with-line-endings/).

# Configure RStudio to use `git`

Now that `git` is installed and configured, you can configure RStudio to use it. This will allow you to drive `git` from RStudio, without necessarily using the command line.

1. Open RStudio
1. Under the **Tools** menu, select **Global Options**
1. Click on **Git/SVN** in the resulting Options window
1. Select **Enable version control interface for RStudio projects**
1. Next to **Git executable** click **Browse**
1. Navigate to `C:\Program Files\Git\bin\git.exe` and click **Open**. This is where Chocolatey installed `git`.
1. Click the **Apply** button and restart RStudio

Now in RStudio, you should see a tab called `Git` in the top right quadrant of the window. This tab is the entrypoint for staging, diffing, committing, and pushing to Github.

Additionally, you can now use RStudio to clone projects from Github:

1. Under the **File** menu select **New Project**
1. On the resulting window select **Version Control**
1. On the window after that, select **Git**, and paste in the clone URL from the project's Github page

# Conclusion

Congrats! You're all set up to develop R applications, develop R packages, and
interact with Github with the best of'em. From here, you can install standard R
tools like the `devtools` and `usethis` packages.

For an introduction to R package development, RStudio's [Developing Packages
with
RStudio](https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-RStudio)
is a good introduction.

Thanks for reading, I hope you have fun writing and sharing code!

