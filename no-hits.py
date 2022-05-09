#!/usr/bin/python3

# This script helps identifying which rules were not applied by the
# test harness.  First, run `make fresh-test-coverage` and then this
# script, by simply typing `no-hits.py -r | less`, assuming
# `no-hits.py` is executable and `python3` is available as in the
# first line. It should print the rules that were not applied.  By
# typing `no-hits.py -t` the same information is printed in the form
# of a table (actually a pretty-printed python3 dictionary) with the file
# names and the line numbers of each rule that was not applied.

import pprint
import sys

h = open(".build/coverage.xml", "r")
data = h.readlines()
h.close()

no_hits = dict()
for line in data:
    l = line.strip()
    if l.find("<class") > -1:
        index_b = l.find("name=")
        index_e = l.find(".md", index_b + 6)
        file_name = l[index_b+6:index_e+3]
        no_hits.update({file_name:[]})
    if l.find("hits=\"0\"") > -1:
        line_num_idx_b = l.find("number=\"")
        line_num_idx_e = l.find("\"", line_num_idx_b + len("number=\""))
        line_num = l[line_num_idx_b+len("number=\""):line_num_idx_e]
        no_hits[file_name].append(line_num)

if "-t" in sys.argv:
    pp = pprint.PrettyPrinter(indent=4)
    pp.pprint(no_hits)

if "-r" in sys.argv:
    for file_name in no_hits.keys():
        if file_name:
            rules = no_hits[file_name]
            if rules != []:
                h = open(file_name, "r")
                line_rules = h.readlines()
                h.close()
                print("=== " + file_name)
                for r in no_hits[file_name]:
                    print("line: " + r + line_rules[int(r) - 1])

