
GHC=$(HOME)/opt/ghc-7.10.3/bin/ghc

FLAGS= -keep-tmp-files -dsuppress-module-prefixes -ddump-to-file \
  -ddump-core-stats -ddump-simpl-stats -dcore-lint -dcmm-lint -ddump-ds \
  -ddump-simpl -ddump-stg  -ddump-bcos -ddump-cmm -ddump-opt-cmm -ddump-inlinings -rtsopts \
  -ddump-llvm 
# Problems with compilation not terminating:
#  -ddump-asm -ddump-asm-conflicts -ddump-asm-expanded -ddump-asm-liveness -ddump-asm-native -ddump-asm-regalloc -ddump-asm-regalloc-stages -ddump-asm-stats \
  -ddump-llvm 

# -ddump-asm                     -ddump-cmm                     -ddump-cmm-split               -ddump-hi-diffs                -ddump-parsed                  -ddump-simpl-iterations        -ddump-ticked
# -ddump-asm-conflicts           -ddump-cmm-cbe                 -ddump-core-stats              -ddump-hpc                     -ddump-prep                    -ddump-simpl-stats             -ddump-to-file
# -ddump-asm-expanded            -ddump-cmm-cfg                 -ddump-cs-trace                -ddump-if-trace                -ddump-rn                      -ddump-simpl-trace             -ddump-types
# -ddump-asm-liveness            -ddump-cmm-cps                 -ddump-cse                     -ddump-inlinings               -ddump-rn-stats                -ddump-spec                    -ddump-vect
# -ddump-asm-native              -ddump-cmm-info                -ddump-debug                   -ddump-llvm                    -ddump-rn-trace                -ddump-splices                 -ddump-view-pattern-commoning
# -ddump-asm-regalloc            -ddump-cmm-proc                -ddump-deriv                   -ddump-minimal-imports         -ddump-rtti                    -ddump-stg                     -ddump-vt-trace
# -ddump-asm-regalloc-stages     -ddump-cmm-procmap             -ddump-ds                      -ddump-mod-cycles              -ddump-rule-firings            -ddump-stranal                 -ddump-worker-wrapper
# -ddump-asm-stats               -ddump-cmm-raw                 -ddump-file-prefix             -ddump-mod-map                 -ddump-rule-rewrites           -ddump-strsigs
# -ddump-bcos                    -ddump-cmm-sink                -ddump-foreign                 -ddump-occur-anal              -ddump-rules                   -ddump-tc
# -ddump-call-arity              -ddump-cmm-sp                  -ddump-hi                      -ddump-opt-cmm                 -ddump-simpl                   -ddump-tc-trace

all: Hello.exe

# OPT= -O0
OPT= -O2

Hello.exe: Hello.hs
	$(GHC) $(OPT) -fforce-recomp $(FLAGS) -v $^ -o $@

clean:
	rm -f Hello.exe *.hi *.o *.dump* *~

