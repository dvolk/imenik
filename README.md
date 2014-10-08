imenik
======

opis: demo internega imenika

operacije: dodajanje, brisanje, spreminjane, iskanje

programski jezik: Haskell (glej imenik.cabal)

baza: sqlite3

web: enostaven cgi

nastavitev:

    $ cd web/server/path
    $ sqlite3 imenik.db
    ( CREATE TABLE imenik (id integer primary key, ime text, priimek text, telefon text); )
    $ vi imenik.hs
    ( nastavite exe_path in db_path v Main.hs )
    $ mkdir -p cgi-bin
    $ ghc ../Main.hs -o imenik

    $ python -m CGIHTTPServer &
    $ firefox http://127.0.0.1:8000
