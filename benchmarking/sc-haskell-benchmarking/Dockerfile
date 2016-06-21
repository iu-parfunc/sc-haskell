FROM parfunc/sc-haskell:v0.3
MAINTAINER Ryan Scott

RUN apt-get update && apt-get install -y \
     faketime

ADD lorem-ipsum.txt /root/lorem-ipsum.txt

ADD entrypoint.sh /root/entrypoint.sh
RUN chmod +x /root/entrypoint.sh

ENTRYPOINT ["/root/entrypoint.sh"]
CMD ["/bin/bash"]
