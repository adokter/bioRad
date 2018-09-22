# CONTRIBUTING #

Contributions are welcome, and they are greatly appreciated! Every little bit helps, and credit will always be given.

You can contribute in many ways:

## Types of Contributions

### Bugs?

* Submit an issue on the Issues page [here](https://github.com/adokter/bioRad/issues)

If you are reporting a bug, please include:

* Your operating system name and version.
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug.

### Fix bugs

Look through the GitHub issues for bugs. Anything tagged with "bug" and "help wanted" is open to whoever wants to implement it.

### Write Documentation

`bioRad` could always use more documentation, whether as part of the official `bioRad` docs, in roxygen strings, or even on the web in blog posts, articles, and such.

### Submit Feedback

The best way to send feedback is to file an issue at https://github.com/adokter/bioRad/issues

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.
* Remember that this is a volunteer-driven project, and that contributions are welcome :)

## Get Started!

Ready to contribute? Here's how to set up `bioRad` for local development.

1. Fork the `bioRad` repo on GitHub.

2. Clone your fork locally:

     ```basic
     $ git clone git@github.com:your_name_here/bioRad.git
     ```

3. Create a branch for local development:

    ```basic
    $ git checkout -b name-of-your-bugfix-or-feature
    ```

4. When you're done making changes (please include unit tests as well), make a check on the code behavior and aim for 0 errors and 0 warnings. In R, you can use the check of the `devtools` package:

    ```r
    devtools::check(document = FALSE, cleanup = FALSE, args = c('--as-cran'))
    ```

5. Commit your changes and push your branch to GitHub. Commit regularly to clarify the individual steps.

    ```basic
    $ git add .
    $ git commit -m "Your detailed description of your changes."
    $ git push origin name-of-your-bugfix-or-feature
    ```

6. Submit a pull request through the [GitHub website](https://github.com/adokter/bioRad/pulls).

### Thanks for contributing!
