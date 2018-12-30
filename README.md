# About

This is the superset disassembler project, based on probabilistic disassembly. The objective is to disassemble a binary or library with very high accuracy but with a guarantee of no missing instructions that were intended by the compiler. It is known there are some additional instructions that are not intended by the compiler and which may even overlap with the true instructions. However, the intended use of this disassembler is with superset rewriters on binaries, where there is a mitigation mechanism in place for false positives in general, so that this is not of concern.

* True positives are instructions intended by the compiler that are correctly in the output of the disassembler.
* False negatives are instructions intended by the compiler but missed by the disassembler.
* False positives are instructions not intended by the compiler, but still contained in the output of the disassembler. 
  * There are two categories of false positives: those that occur inside true positives and those that do not. Those that do are called occlusive, and those that do not are not given much attention as it is thought they do not meaningfully impact considerations other than a negligible size in crease in the rewritten binary.

Pros:
* No false negatives
* Very low false positive rate
* Very well tested
* Amenable to further improvement

Cons:
* Slower than linear sweep
* Requires BAP as a dependency, which itself large
* Requires occlusive false positive mitigation


**An existing docker image is provided for you, where this repository and all dependencies are already built for you. Another image, which builds off of the previous, provides the metrics and artifacts referenced in the paper.** It is simple enough to *simply pull the docker image kennethadammiller/superset_disasm_metrics* to obtain the metrics with calculate_metrics.py as described below. However, the ability to run the analyses to obtain the results independently in a repeatable, replicable, and reproducible manner per the ACM badging policies is of course provided for. The dockerfiles for each are provided with the base image corresponding to 'Dockerfile' and the metrics being in 'Dockerfile.metrics'. The superset_disasm_metrics image is an instance of Dockerfile.metrics having already been run, and if you wish to skip waiting for a run to complete (which takes about at least 8 hours), you can just pull that. 

# How to build

To build for local development, just call make. There is a binary and library produced as main artifacts.

You can use an opam pin to locally pin for your own edits if you so desire with:

`opam pin add superset_disasm ./ -y`

To build for docker, you can do:

`docker build . -t superset_disasm`

Or, (after [downloading the CPU 2006 benchmarks](https://drive.google.com/open?id=1Pcqp7OfkTvlXn-rtIMDUPh4IjdBrixEs) modified to run the rewritten binaries by skipping the hash check) you can run the metrics automatically with:

`docker build . -f Dockerfile.metrics -t superset_disasm_metrics`


# How to run the scripts manually
The scripts are in the ./scripts folder. There are lots of helper scripts, so if you looked in to orient yourself, you should likely have to read them to know the structure, which is not advised unless you want to develop with the project. Instead, below you will find the synopsis of each script and how they work. The most important are collect_results.sh and calculate_metrics.py

* run_analyses.sh - This script demonstrates the different non-probabilistic analyses by gathering metrics while running them each individually. No probabilistic analyses are run.
* run_features.sh - This script demonstrates the different probabilistic analyses by running each individual specific feature used to converge with the fixpoint.
* collect_results.sh - This script parses the results of all the analyses into a set of files that can then be processed by python. It produces many text files with simple numbers line by line.
* calculate_metrics.py - This script reads the files produced by collect_results.sh and outputs a set of human readable percentages.
* cleanup.sh - This erases the results of an analysis run over the whole corpora.
* linear_sweep.sh - Runs and parses the result of a linear sweep on the corpora
* calc_fn.py - Uses the ground truth from unstripped binaries and the addresses of both linear sweep and the superset disassemblers to calculate the number of false negatives, if any.
* collect_fns.sh - Is a script that calls calc_fn.py for each of the target binaries in the entire corpora.

The other scripts aren't as important, but effectively they can be run in that order, and the only script that requires parameters is calc_fn, but collect_fns.sh will run that for you.

# Exposition on produced artifacts
Every binary analyzed by the metrics gathering tool produces many different metrics. These metrics display the number of false positives, the maximum possible total number of occlusive false positives, false negatives, and many other details. This output goes into a file, named by the convention <binary_name>.metrics. These are parsed with grep and awk style tools to retrieve the results.

In so much as the results produced by the actual disassembler, it can yield the following byproducts: the reverse instruction sequence graph, the instruction interval map (BAP insn type cannot be saved, but the most used meta data can be retained), the ground truth address set from the unstripped binary, and the addresses produced by the superset disassembler itself. The metrics output is printed to standard output, which in the analysis script is redirected to \*.metrics for each binary analyzed. The artifacts and measurements from the paper are below: 

start the docker image superset_disasm_metrics...

1. RQ1, we perform four experiments:  
   1. measure false negatives (missing true positive instructions) false positives (bogus instructions) 
       * run the command `python calculate_metrics.py` in each of the respective folders in `${HOME}/workspace/superset_disasm/` that have a `metrics.txt` file to retrieve the fundamental statistics for those settings
       * The text files `false_negatives.txt` and `false_positives.txt` contain the output for each file as reported by the disassembler tool.
       * **Fig. 7** was produced from the files false_positives.txt and mem_size.txt
   1. measure the disassembling time
       * The time and binary size is collected in the files `times.txt` and `mem_size.txt` and these were used to make **Fig. 8**
   1. analyze the contributions of each individual kind of probabilistic hints
       * The folders `${HOME}/workspace/superset_disasm/` that have a `metrics.txt` file were each run with different hints, which are specified in the folder name. **Fig 9** is made with the output of calculte_metrics.py run in each of these folders.
   1. study the effect of different probability threshold settings.
       * The folders `${HOME}/workspace/superset_disasm/` that have a `metrics.txt` file include different probability threshold values. That value is in the folder name. This data was used to make **Fig. 10**.
1. RQ2: Comparison with Superset Disassembly
   * The files `${HOME}/workspace/multiverse/*_rewrite.txt` contain the output of the rewriter. One of those lines has the percentage blow up in size.
   * The results of the SPEC CPU benchmark for the original spec binaries and for the rewritten binaries is in `${HOME}/workspace/cpu2006-103/result`, numbered in the order they were run which is original and then rewritten. **Table II** is made from this data.
1. RQ3: Handling Data and Code Interleavings - windows binary tests
   * This data is
   * **Table III** is made from this data.
1. RQ4: Handling Missing Function Entries
   * The folder in `${HOME}/workspace/superset_disasm/` that contains the string DiscardEdges as part of it's feature set contains the result of disassembling without any edges to provide for grammar recognition.
