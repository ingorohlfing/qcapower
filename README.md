Resources for preparing the package for CRAN submission

- goodpractice package: <https://github.com/mangothecat/goodpractice>
- checking package on Windows with different R versions: <https://win-builder.r-project.org/>
- checking package on Macs
- Wickham/Bryan book on packages: <https://r-pkgs.org/>
- CRAN package policies: <https://cran.r-project.org/web/packages/policies.html>
- A short intro to the submission process: <https://kbroman.org/pkg_primer/pages/cran.html>


 - goodpractices to do list:
 It is good practice to

  ✖ add a "URL" field to DESCRIPTION. It helps users find information about your package online. If
    your package does not have a homepage, add an URL to GitHub, or the CRAN package package page.
  ✖ add a "BugReports" field to DESCRIPTION, and point it to a bug tracker. Many online code hosting
    services provide bug trackers for free, https://github.com, https://gitlab.com, etc.
  ✖ not import packages as a whole, as this can cause name clashes between the imported packages.
    Instead, import only the specific functions you need.
