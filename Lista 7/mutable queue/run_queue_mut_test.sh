#!/bin/bash

ocamlc -c queue_mut.mli
ocamlc -c queue_mut.ml
ocamlc -c queue_mut_test.ml
ocamlc -o queueTest queue_mut.cmo queue_mut_test.cmo
ocamlrun queueTest
