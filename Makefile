
VER=v0.5

NAME1= parfunc/sc-haskell:$(VER)

docker: dock1 dock2

dock1: 
	docker build -t $(NAME1) -f Dockerfile .
	docker run -it $(NAME1) /usr/bin/ghc --version

dock2:

push: push1

push1:
	docker push $(NAME1)
