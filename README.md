# Otter
 It can be difficult for students and developers to maintain good style and practices when writing OCaml code. Otter serves as a linter for OCaml files, identifying and fixing style issues based on the JHU FPSE course style guide.

To use Otter, the user can provide a file that they want to format in the command line. Our current implementation extracts, parses, and abstracts the file contents into code blocks, which are stored in records. Otter then formats the blocks in order of sequence number, and outputs the adjusted contents to a new file.

The user can run Otter in the following way, with optional flags.

1. dune build
2. dune exec -- lib/otter.exe [/file_path/file.ml] [flags]

Flags
- --indent-size: The number of spaces to indent each line of code. Default is 2.
- --column-width: The maximum number of characters per line. Default is 80.

Example call: dune exec -- lib/otter.exe /file_path/file.ml --indent-size 3 --column-width 20

Testing:
- dune test