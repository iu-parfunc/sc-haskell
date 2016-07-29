
all: report_sc-v0.4.html report_sc-v0.4-dbg.html report_sc-v0.4-opt.html report_sc-v0.4-opt-dbg.html report_stock.html

dbg: report_sc-v0.4-dbg.html report_sc-v0.4-opt-dbg.html 

ITERS=10000

report_sc-v0.4.html:
#	stack docker pull --docker-image=parfunc/sc-haskell:v0.4
	docker run -it parfunc/sc-haskell:v0.4 ghc --version
	stack --docker --docker-image=parfunc/sc-haskell:v0.4 bench --benchmark-arguments="-o $@ --csv report_sc-v0.4.csv"

report_sc-v0.4-dbg.html:
#	stack docker pull --docker-image=parfunc/sc-haskell:v0.4-dbg
	docker run -it parfunc/sc-haskell:v0.4-dbg ghc --version
# Ludicrous problem with argument parsing: -o $@ --csv report_sc-v0.4-dbg.csv 
	stack --docker --docker-image=parfunc/sc-haskell:v0.4-dbg bench --benchmark-arguments="-n$(ITERS)"
	mv Handle.eventlog Handle_sc-v0.4-dbg.eventlog

report_sc-v0.4-opt.html:
#	stack docker pull --docker-image=parfunc/sc-haskell:v0.4-opt
	docker run -it parfunc/sc-haskell:v0.4-opt ghc --version
	stack --docker --docker-image=parfunc/sc-haskell:v0.4-opt bench --benchmark-arguments="-o $@ --csv report_sc-v0.4-opt.csv"

report_sc-v0.4-opt-dbg.html:
#	stack docker pull --docker-image=parfunc/sc-haskell:v0.4-opt-dbg
	docker run -it parfunc/sc-haskell:v0.4-opt-dbg ghc --version
# same problem: -o $@ --csv report_sc-v0.4-opt-dbg.csv 
	stack --docker --docker-image=parfunc/sc-haskell:v0.4-opt-dbg bench --benchmark-arguments="-n$(ITERS)"
	mv Handle.eventlog Handle_sc-v0.4-opt-dbg.eventlog

report_stock.html:
#	stack docker pull --docker-image=fpco/stack-build:lts-5.16
	docker run -it fpco/stack-build:lts-5.16 ghc --version
	stack --compiler=ghc-7.10.3 --docker --docker-image=fpco/stack-build:lts-5.16 bench --benchmark-arguments="-o $@ --csv report_stock.csv"

distclean:
	rm -f report*.html report*.csv
