Resources for preparing the package for CRAN submission

- goodpractice package: <https://github.com/mangothecat/goodpractice>
- checking package on Windows with different R versions: <https://win-builder.r-project.org/>
- checking package on Macs
- Wickham/Bryan book on packages: <https://r-pkgs.org/>
- CRAN package policies: <https://cran.r-project.org/web/packages/policies.html>
- A short intro to the submission process: <https://kbroman.org/pkg_primer/pages/cran.html>


 - goodpractices to do list:
 It is good practice to
 

  
  ✖ not import packages as a whole, as this can cause name clashes between the imported packages. Instead, import only the specific functions you need.
