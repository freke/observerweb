FROM codesimple/elm:0.18

RUN npm install -g bower
RUN npm install -g elm-analyse
RUN npm install -g elm-format@exp

EXPOSE 3000

ENTRYPOINT []
