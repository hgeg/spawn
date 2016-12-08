#!/usr/bin/env python
from flask import Flask
from flup.server.fcgi import WSGIServer

app = Flask(__name__)

@app.route('/')
def main(): return "It Works!"

if __name__ == "__main__":
    WSGIServer(app).run()
