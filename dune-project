(lang dune 3.4)
(name otter)

(generate_opam_files true)
(authors "Matthew Liu" "Will Tong" "JJ Yang")
(maintainers "mliu78@jhu.edu" "wtong10@jhu.edu" "kyang53@jhu.edu")
(package (allow_empty)
 (name otter) ; put in your own project name here
 (synopsis "An OCaml library for formatting OCaml code")
 (description "It can be difficult for students and developers to maintain good style and practices when writing OCaml code. Otter serves as a linter for OCaml files, identifying and fixing style issues based on the JHU FPSE course style guide.")
 (depends
  (ounit2 (> 2.2)) ; for each opam dependency list the version
  (core (> 0.15.0)) ; `opam list core` will display which version of core you have
))
