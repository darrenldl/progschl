# progschl
Programmers Schedule

## Note
The project is still in progress, this repo is copied from Bitbucket repo and will not be the main development repo.

Currently pscl reads from test-run.pscl and tries to interpret it, and will either display errors or produce a (not user-friendly) output of the interpreted/analyzed data.

## Install and Execution
Install Racket : https://download.racket-lang.org/

#### Install dependencies
    raco pkg install functional-lib
    raco pkg install megaparsack
    raco pkg install gregor-lib

#### Compile to bytecode
    raco make src/pscl.rkt
    
#### Run
    racket src/pscl.rkt

#### Program structure
Currently, progschl consists of 2 major components :
  - file parser (primary file parsing)
  - analyzer (deeper file analysis)
  
The last component to be completed :
  - time point generator (generates tasks on timeline)
