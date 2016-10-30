#!/usr/bin/env python
import sys
from flask import Flask
app = Flask(__name__)
@app.route('/')
def main(): return "It Works!"
if __name__ == "__main__":
    args = dict(zip(sys.argv[1::2],sys.argv[2::2]))
    if "-p" not in args: print "argument error"
    else: app.run("localhost",int(args["-p"]))
