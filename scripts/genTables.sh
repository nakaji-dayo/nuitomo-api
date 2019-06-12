#!/bin/bash
set -eu

export PGPASSWORD=passwd

DB=${POSTGRES_DATABASE:-nuitomo-api}

entityImport=./src/Entity.hs
entityImportDummies=./dummies.tmp
rm -f $entityImportDummies
touch $entityImportDummies


mkdir -p ./src/Entity
echo "module Entity" > $entityImport

tail='f'

rm -f ./src/Entity/*

psql -h localhost -d ${DB} -U api -Atc "select tablename from pg_tables where schemaname='public' order by tablename" | \
    while read entity; do
        model=`echo $entity | perl -pe "s/_(.)/\u\1/g; s/^(.)/\u\1/g"`
        lmodel=`echo $model | perl -ne 'print lcfirst'`
        if [ $model != "SchemaMigrations" ]; then
            cat > ./src/Entity/${model}.hs << EOS
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Entity.${model} where

import           DataSource   (defineEntity)
import           Prelude      hiding (id)
import           GHC.Generics
import           TH.Type
import           Instance()
import           Data.Generics.Labels ()

\$(defineEntity "${entity}" [''Show, ''Generic, ''Eq])

\$(deriveApiField ''${model})
EOS

            echo -n "  " >> $entityImport
            if [ $tail == 't' ]; then
                echo -n ", " >> $entityImport
            else
                echo -n "( " >> $entityImport
            fi
            tail='t'

            printf "module Entity.${model}\n" >> $entityImport
            printf "import Entity.${model} (${model} (${model}), ${lmodel})\n" >> $entityImportDummies
            printf "import qualified Entity.${model} ()\n" >> $entityImportDummies
        fi
    done

printf "  ) where\n\n" >> $entityImport

cat $entityImportDummies >> $entityImport
rm $entityImportDummies
