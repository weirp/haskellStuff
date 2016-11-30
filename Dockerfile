FROM library/ubuntu:latest
MAINTAINER Philip J Weir <weirp@acm.org>

# to build:
#    docker build --rm -t weirp/haskell-stack .
# to run:
#    docker run -t -i weirp/haskell-stack bash

RUN apt-get update
RUN apt-get install -y apt-utils
RUN apt-get install -y emacs
RUN apt-get install -y wget

# see https://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442

RUN echo 'deb http://download.fpcomplete.com/ubuntu yakkety main'|tee /etc/apt/sources.list.d/fpco.list

RUN apt-get update
RUN apt-get install stack -y

# see stackage.org

RUN stack setup

RUN echo "export PATH=`stack path|grep ^bin-path|awk '{print $2}'`:$PATH" >> ~/.bashrc


# wreq
# www.serpentine.com/wreq
# hackage -- search for wreq

RUN stack build wreq




# up to 23:34
