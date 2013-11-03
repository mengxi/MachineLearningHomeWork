"""Module docstring

This is for change R data type so that it can be load in R version > 3

Author: Ji Zhu
Data: 2013/11/1

"""

import sys
import getopt

def reformat_R(filename, output):
  mynumbers = []
  with open(filename) as f:
    for line in f:
      mynumbers.append([float(n) for n in line.strip().split('\t')])

  with open(output,'w') as f:
    for numbers in mynumbers:
      for n in numbers:
        if n < 1e-5: f.write("%d, " % n)
        else: f.write("%0.2f, " % n)
      f.write("\n")

def main():
  # parse command line options
  try:
    opts, args = getopt.getopt(sys.argv[1:], "h", ["help"])
  except getopt.error, msg:
    print msg
    print "for help use --help"
    sys.exit(2)
  # process options
  for o, a in opts:
    if o in ("-h", "--help"):
      print __doc__
      sys.exit(0)
  # process arguments
  print args
  reformat_R(args[0], args[1])

if __name__ == "__main__":
  main()

