
VER=v0.5


all: report_sc-$(VER).html report_sc-$(VER)-dbg.html report_sc-$(VER)-opt.html report_sc-$(VER)-opt-dbg.html report_stock.html

dbg: report_sc-$(VER)-dbg.html report_sc-$(VER)-opt-dbg.html 

ITERS=10000

report_sc-$(VER).html:
#	stack docker pull --docker-image=parfunc/sc-haskell:v0.4
	docker run -it parfunc/sc-haskell:$(VER) ghc --version
	stack --docker --docker-image=parfunc/sc-haskell:$(VER) bench --benchmark-arguments="-o $@ --csv report_sc-$(VER).csv"

report_sc-$(VER)-dbg.html:
#	stack docker pull --docker-image=parfunc/sc-haskell:$(VER)-dbg
	docker run -it parfunc/sc-haskell:$(VER)-dbg ghc --version
# Ludicrous problem with argument parsing: -o $@ --csv report_sc-$(VER)-dbg.csv 
	stack --docker --docker-image=parfunc/sc-haskell:$(VER)-dbg bench --benchmark-arguments="-n$(ITERS)"
	mv Handle.eventlog Handle_sc-$(VER)-dbg.eventlog

report_sc-$(VER)-opt.html:
#	stack docker pull --docker-image=parfunc/sc-haskell:$(VER)-opt
	docker run -it parfunc/sc-haskell:$(VER)-opt ghc --version
	stack --docker --docker-image=parfunc/sc-haskell:$(VER)-opt bench --benchmark-arguments="-o $@ --csv report_sc-$(VER)-opt.csv"

report_sc-$(VER)-opt-dbg.html:
#	stack docker pull --docker-image=parfunc/sc-haskell:$(VER)-opt-dbg
	docker run -it parfunc/sc-haskell:$(VER)-opt-dbg ghc --version
# same problem: -o $@ --csv report_sc-$(VER)-opt-dbg.csv 
	stack --docker --docker-image=parfunc/sc-haskell:$(VER)-opt-dbg bench --benchmark-arguments="-n$(ITERS)"
	mv Handle.eventlog Handle_sc-$(VER)-opt-dbg.eventlog

report_stock.html:
#	stack docker pull --docker-image=fpco/stack-build:lts-5.16
	docker run -it fpco/stack-build:lts-5.16 ghc --version
	stack --compiler=ghc-7.10.3 --docker --docker-image=fpco/stack-build:lts-5.16 bench --benchmark-arguments="-o $@ --csv report_stock.csv"

distclean:
	rm -f report*.html report*.csv
