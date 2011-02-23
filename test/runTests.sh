#!/bin/bash

cd $(dirname $0)

runhaskell -i./ -i../ test.hs

