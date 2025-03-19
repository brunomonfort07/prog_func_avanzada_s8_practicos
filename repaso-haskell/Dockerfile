FROM haskell:9
WORKDIR /
RUN cabal update;
RUN cabal install BNFC;
RUN cabal install alex;
RUN cabal install happy;
RUN cabal install haskell-language-server
RUN cabal install hoogle
RUN cabal install hspec-discover

RUN mkdir /pfunc
COPY . pfunc
WORKDIR pfunc

CMD /bin/bash
