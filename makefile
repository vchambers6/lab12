# Assignment specific setup

# Directories
ASN := lab12
VOCASN := LAB12

# A list of all the file stems for OCaml files that have both
# scaffold and solution versions
SOURCES := lab12.ml
TESTED := lab12
# Files to be distributed to students in the work directory
DISTRIB := lab12.ml _tags cS51.ml
SUBMISSION := lab12.ml

# General rules
include ../../makefile
