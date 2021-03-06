
VER=v0.5

NAME1= parfunc/sc-haskell:$(VER)
NAME2= parfunc/sc-haskell:$(VER)-dbg
NAME3= parfunc/sc-haskell:$(VER)-opt
NAME4= parfunc/sc-haskell:$(VER)-opt-dbg
NAME5= parfunc/sc-haskell:$(VER)-readbarrier

dockall: dock1 dock2 dock3 dock4 dock5
pushall: push1 push2 push3 push4 push5

# Build four different docker images
# ================================================================================

dockerfiles: Dockerfile Dockerfile_dbg Dockerfile_opt Dockerfile_opt_dbg Dockerfile_readbarrier

# * wip/sc-ghc-7.10 branch: 2e98e616dc217b92dd19eeb1f781271e440aad5a
# * same branch, update version to 7.10.4, sc-haskell release v0.4:
#    0.4 at 71694066e532731c04a4a37e56b7590f555a01e8 was actually broken,
#    because it was really "-opt".  I.e. it lacked the Handle barriers.
# * Update to 0.5: 181e760138a277fc9cd2b1b3547bc0e93757dc22
#   - To do so, kick (just) Handle out of the trusted codebase, make it use fences.
Dockerfile:
	sed 's/REPLACE_ME_WITH_SHA/181e760138a277fc9cd2b1b3547bc0e93757dc22/' Dockerfile.in > $@

# wip/sc-ghc-7.10-no-opt-dbg branch:
# [2016.07.16] Second attempt 0cbec5f521820bd8c88719055c28b0ee88a0439b
# 0.4 was wrong/broken at 0cbec5f521820bd8c88719055c28b0ee88a0439b
# Here's 0.5-dbg to fix it: 4af1fad50443a89e844f827f4829f5eab3b49d4d
Dockerfile_dbg:
	sed 's/REPLACE_ME_WITH_SHA/4af1fad50443a89e844f827f4829f5eab3b49d4d/' Dockerfile.in > $@

# * v0.4-opt, with the Handle barrier elision: I think this one was actually correct.
#   v0.4-opt = 012e9df498d87cfd1134948348378a56c7020eae
Dockerfile_opt:
#	sed 's/REPLACE_ME_WITH_SHA//' Dockerfile.in > $@

# WAS wip/sc-ghc-7.10-dbg branch:
# * same branch, update version to 7.10.4, sc-haskell release v0.4: 7bc4fd6c3346914bc5c2c16f4f86e519acf5c2e5
#   I think this one was actually correct in the 0.4 version.
Dockerfile_opt_dbg:
#	sed 's/REPLACE_ME_WITH_SHA//' Dockerfile.in > $@

# Read barriers
# [2016.10.11] 3c009f104a4ec7d6fa2aff86b9e5f3c5599005b8
Dockerfile_readbarrier:
	sed 's/REPLACE_ME_WITH_SHA/3c009f104a4ec7d6fa2aff86b9e5f3c5599005b8/' Dockerfile.in > $@

#----------------------------------------

dock1: Dockerfile
	docker build -t $(NAME1) -f Dockerfile .
	docker run -it $(NAME1) /usr/bin/ghc --version

dock2: Dockerfile_dbg
	docker build -t $(NAME2) -f Dockerfile_dbg .
	docker run -it $(NAME2) /usr/bin/ghc --version

dock3: Dockerfile_opt
	docker build -t $(NAME3) -f Dockerfile_opt .
	docker run -it $(NAME3) /usr/bin/ghc --version

dock4: Dockerfile_opt_dbg
	docker build -t $(NAME4) -f Dockerfile_opt_dbg .
	docker run -it $(NAME4) /usr/bin/ghc --version

dock5: Dockerfile_readbarrier
	docker build -t $(NAME5) -f Dockerfile_readbarrier .
	docker run -it $(NAME5) /usr/bin/ghc --version

push1:
	docker push $(NAME1)

push2:
	docker push $(NAME2)

push3:
	docker push $(NAME3)

push4:
	docker push $(NAME4)

push5:
	docker push $(NAME5)

clean:
	rm Dockerfile Dockerfile_dbg

