This is the superset disassemblers project, based on probabilistic disassembly. The objective is to disassemble with very high accuracy but with a guarantee of no missing instructions that were intended by the compiler. It is known there are some additional instructions that are not intended by the compiler and which may even overlap with the true instructions. However, the intended use of this disassembler is with superset rewriters on binaries, where there is a mitigation mechanism in place for false positives in general, so that this is not of concern.

True positives are instructions intended by the compiler that are correctly in the output of the disassembler.
False negatives are instructions intended by the compiler but missed by the disassembler.
False positives are instructions not intended by the compiler, but still contained in the output of the disassembler. There are two categories of false positives: those that occur inside true positives and those that do not. Those that do are called occlusive, and those that do not are not given much attention as it is thought they do not meaningfully impact considerations other than a negligible size in crease in the rewritten binary.

Pros:
	This disassembler is regarded to be quite accurate, having been tested on the BAP elf binaries and a huge number of the cygwin binaries without yielding false negatives, and also producing a very amenably low number of occlusive false positives.
Cons:
	It can be quite slow, and it requires a BAP installation to build, which is large and has quite a few dependencies and takes time to build. 


*An existing docker image is provided for you, where this repository and all dependencies are already built for you. Another image, which builds off of the previous, provides the metrics and artifacts referenced in the paper.* It is simple enough to simply pull the docker image superset_disasm_metrics to obtain the metrics with calculate_metrics.py as described below. However, the ability to run the analyses to obtain the results independently in a repeatable, replicable, and reproducibly per the ACM badging policies is of course provided for. The dockerfiles for each are provided with the base image corresponding to 'Dockerfile' and the metrics being in 'Dockerfile.metrics'. The superset_disasm_metrics image is an instance of Dockerfile.metrics having already been run, and if you wish to skip waiting for a run to complete (which takes about at least 8 hours), you can just pull that. 

#How to build
    To build for local development, just call make. There is a binary and library produced as main artifacts.
    You can use an opam pin to locally pin for your own edits if you so desire with:
       opam pin add superset_disasm ./ -y
    To build for docker, you can do:
       docker build . -t superset_disasm
    Or, you can run the metrics automatically with:
    	docker build . -f Dockerfile.metrics -t superset_disasm_metrics

#How to run the scripts manually
    The scripts are in the ./scripts folder. There are lots of helper scripts, so if you looked in to orient yourself, you should likely have to read them to know the structure, which is not advised unless you want to develop with the project. Instead, below you will find the synopsis of each script and how they work. The most important are collect_results.sh and calculate_metrics.py
    	run_analyses.sh - This script demonstrates the different non-probabilistic analyses by gathering metrics while running them each individually. No probabilistic analyses are run.
	run_features.sh - This script demonstrates the different probabilistic analyses by running each individual specific feature used to converge with the fixpoint.
    	collect_results.sh - This script parses the results of all the analyses into a set of files that can then be processed by python. It produces many text files with simple numbers line by line.
	calculate_metrics.py - This script reads the files produced by collect_results.sh and outputs a set of human readable percentages.
	cleanup.sh - This erases the results of an analysis run over the whole corpora.
	linear_sweep.sh - Runs and parses the result of a linear sweep on the corpora
	calc_fn.py - Uses the ground truth from unstripped binaries and the addresses of both linear sweep and the superset disassemblers to calculate the number of false negatives, if any.
	collect_fns.sh - Is a script that calls calc_fn.py for each of the target binaries in the entire corpora.
    The other scripts aren't as important, but effectively they can be run in that order, and the only script that requires parameters is calc_fn, but collect_fns.sh will run that for you.

#Exposition on produced artifacts
    Every binary analyzed by the metrics gathering tool produces many different metrics. These metrics display the number of false positives, the maximum possible total number of occlusive false positives, false negatives, and many other details. This output goes into a file, named by the convention <binary_name>.metrics. These are parsed with grep and awk style tools to retrieve the results.
    In so much as the results produced by the actual disassembler, it can yield the following byproducts: the reverse instruction sequence graph, the instruction interval map (BAP insn type cannot be saved, but the most used meta data can be retained), the ground truth address set from the unstripped binary, and the addresses produced by the superset disassembler itself.