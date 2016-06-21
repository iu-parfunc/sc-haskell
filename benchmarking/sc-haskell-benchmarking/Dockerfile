FROM parfunc/sc-haskell:v0.3
MAINTAINER Ryan Scott

WORKDIR /root

RUN apt-get update && apt-get install -y \
     faketime

RUN wget https://raw.githubusercontent.com/iu-parfunc/sc-haskell/bf2559bc0d4fd628ba3e05fb91ca25d8788fdba3/benchmarking/lorem-ipsum.txt

WORKDIR /

ADD entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
CMD ["/bin/bash"]
