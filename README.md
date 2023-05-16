GETTING STARTED
===============

0. We assume [OCaml](https://ocaml.org/), [Dune](https://dune.build), and the
   [Z3 SMT solver](https://github.com/Z3Prover/z3/) are installed.

1. To build and install (to local opam profile) the automata library, `nerode`:

        $ cd nerode
        $ opam install --deps-only .
        $ dune build
        $ dune install

2. To build the learning library, `nerode-learn`:

        $ cd nerode-learn
        $ opam install --deps-only .
        $ opam pin add z3 https://github.com/mdmoeller/ocaml-z3.git  # install auxiliary ocaml-z3 bindings
        $ dune build

3. Some notes about organization:
    * We provide bash scripts for reproducing the various experiments in
      section 9 of the paper in `scripts/`. In particular,
      `scripts/reproduce-abridged.sh` and
      `scripts/reproduce-everything.sh` make a new
      directory, `output/`, and produce CSV files into
      that directory with the corresponding data.
    * Unit tests for `nerode` and `nerode-learn` can be run with the command
      `make test` from either `nerode/` or `nerode-learn/`, respectively.
    * Documentation for `nerode` and `nerode-learn` can be built with the
      command `make doc` from either `/home/opam/imat-sorce/nerode` or
      `/home/opam/imat-source/nerode-learn`. The result is odoc-generated html
      documentation in a directory called `docs` in each library directory,
      respectively.

4. After building `nerode` and `nerode-learn`, here are two simple commands to
   verify that things are working:
    
        $ dune exec nerodelearn -- lsblanks-sep "00*" "11*"

            For this example, L+ = 00* and L- = 11*. The system should learn a 2 state DFA:

               | 0 1    <--- these are the alphabet symbols
            ---+----
             0 | 1 0    <--- this is the start state (always 0)
             1 | 1 0 *  <--- this is an accepting state (indicated by *)
            Dfa size: 2
            1*0(0+11*0)* <-- corresponding regex (naively converted)

    Or, to run on a finite list of example strings (the flags -s and -pq are,
    respectively the unsat-cores and heuristic prioritization optimizations):

        $ dune exec nerodelearn -- lsblanks -s -pq ../benchmarks/example.txt

            Dfa_size:	3
            Popped_Items:	6
            Query Count:	31
            Conjectures:	3
            Conjecture_Time:	0.001159
            Cols_Update_Time:	0.000022
            BlankSMT_Time:	0.222892
            Learn Time:	0.224165
            Total Popped_Items:	6
            Total Conjectures:	3
            Total Conjecture_Time:	0.001159
            Total Cols_Update_Time:	0.000022
            Total BlankSMT_Time:	0.222892
            Total Learn Time:	0.224165
            
               | 0 1
            ---+----
             0 | 2 1 *
             1 | 1 1 
             2 | 2 2 *
            e+0(0+1)*

5.  Most benchmarks use the alphabet `{0,1}`. When this alphabet is used, input
    files look like this:

        0,+
        00,+
        00X,+
        1,-
        10,-

    Here, the + and - indicated positive and negative examples, respectively,
    and the 'X' serves as a wildcard that expands to both 0 and 1, meaning
    that 000 and 001 are both positive examples.

    Users may also use arbitrary sets of strings as alphabets. For example, a
    user that wants to learn an automaton from traces of method calls in Java
    might want to use the alphabet `{hasNext, next}`:

        hasNext hasNext next;+
        hasNext next hasNext next;+
        hasNext;+
        hasNext   next;+
        next;-
        next hasNext;-
        next next hasNext next;-
        next next next hasNext;-
        hasNext next next;-

    The use of the ';' instead of ',' is important; it signals to the system not
    to assume `{0,1}` and instead to split the input symbols on whitespace. The
    'X' wildcard only applies when the alphabet is `{0, 1}` (and the ',' is used
    for labels).

    This use case example is inspired by [Lee, Chen, and Rosu](https://dl.acm.org/doi/pdf/10.1145/1985793.1985874).
