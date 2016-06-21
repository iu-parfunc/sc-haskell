FROM fpco/stack-build:lts-5.16
MAINTAINER Ryan Scott

# RUN apt-get update && apt-get install -y \
#      faketime

ADD lorem-ipsum.txt /root/lorem-ipsum.txt

ADD entrypoint.sh /root/entrypoint.sh
RUN chmod +x /root/entrypoint.sh

ENTRYPOINT ["/root/entrypoint.sh"]
CMD ["/bin/bash"]
