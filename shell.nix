{ pkgs ? import <nixpkgs> {}, ... }:
let
  inherit (pkgs) stdenv;
in stdenv.mkDerivation {
  name = "tagger-dev";
  buildInputs = with pkgs; [
    stack
    postgresql
    zlib
  ];
  shellHook = ''
    export PGHOST=$PWD/.postgres
    export PGDATA=$PGHOST/data
    export PGDATABASE=postgres
    export PGLOG=$PGHOST/postgres.log

    mkdir -p $PGHOST

    if [ ! -d $PGDATA ]; then
      initdb --auth=trust --no-locale --encoding=UTF8
    fi

    if ! pg_ctl status
    then
      pg_ctl start -l $PGLOG -o "--unix_socket_directories='$PGHOST'"

      echo "creating database"
      psql --command 'create database tagger'

      echo "creating user"
      psql --command 'create user tagger;'

      echo "granting privileges"
      psql --command 'grant all privileges on database tagger to tagger;'
    fi
  '';
}

