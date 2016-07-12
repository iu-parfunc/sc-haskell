

all: report_sc.html report_stock.html

report_sc.html:
	stack --docker --docker-image=parfunc/sc-haskell:v0.3 bench --benchmark-arguments="-o report_sc.html"

report_stock.html:
	stack --docker --docker-image=fpco/stack-build:lts-5.16 bench --benchmark-arguments="-o report_stock.html"
